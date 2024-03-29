use crate::{
    asm::{write_inst_dasm, ADDRESS_COMMENT_TOKEN, INSTRUCTION_MAX_LENGTH},
    ch8::{
        rom::RomConfig,
        vm::{VMHistoryFragment, VM},
    },
};

use crossterm::event::{KeyCode, KeyEvent};
use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Paragraph, Widget},
};

use std::{collections::VecDeque, fmt::Write};

const HISTORY_CAPACITY: usize = 1_000_000;

pub(super) struct History {
    pub fragments: VecDeque<VMHistoryFragment>,
    pub present_fragment: Option<VMHistoryFragment>,
    rom_config: RomConfig,
    cursor: usize,
}

impl History {
    pub(super) fn new(rom_config: RomConfig) -> Self {
        Self {
            rom_config,
            fragments: VecDeque::with_capacity(HISTORY_CAPACITY),
            present_fragment: None,
            cursor: 0,
        }
    }

    pub(super) fn redo_amount(&self) -> usize {
        self.fragments.len().abs_diff(self.cursor)
    }

    pub(super) fn clear_redo_history(&mut self) {
        self.fragments.truncate(self.cursor);
    }

    pub(super) fn undo(&mut self, vm: &mut VM, amt: usize, memory_access_flags: &mut [u8]) -> usize {
        if self.redo_amount() == 0 {
            self.present_fragment = Some(vm.to_history_fragment(memory_access_flags));
        }

        let mut amt_rewinded = 0;
        for _ in 0..amt {
            if self.cursor == 0 {
                break;
            }
            self.cursor -= 1;
            vm.undo(&self.fragments[self.cursor], memory_access_flags);
            amt_rewinded += 1;
        }
        amt_rewinded
    }

    pub(super) fn step(&mut self, vm: &mut VM, memory_access_flags: &mut [u8]) -> Result<bool, String> {
        // time step is not state that is completely deterministic so must set it if possible
        if self.cursor < self.fragments.len() {
            vm.set_cycles_per_frame(self.fragments[self.cursor].cycles_per_frame);
        }

        let state = vm.to_history_fragment(memory_access_flags); // get state of vm

        // if we have redo ahead of us but the cursor isnt consistent with our current state then we need to clear it
        let mut redo_amount = self.redo_amount();
        if redo_amount == 0 {
            self.present_fragment = None;
        }

        if redo_amount > 0 && state != self.fragments[self.cursor] {
            log::info!(
                "Clearing {} history checkpoints at or ahead of cursor",
                redo_amount
            );
            state.log_diff(&self.fragments[self.cursor]); // DEBUG
            self.fragments.truncate(self.cursor);
            self.present_fragment = None;
            redo_amount = 0;
            log::error!("Redo history was cleared during execution step operation because current state did not agree with redo history.");
        }

        let vm_result = vm.stepn(1);

        // if vm is continuing then update memory access flags too
        if let Ok(true) = vm_result {
            if !vm.interpreter().waiting {
                vm.update_memory_access_flags(&state.interpreter, memory_access_flags);
            }
        }

        if redo_amount == 0 && !vm.interpreter().waiting && vm_result.is_ok() {
            if self.fragments.len() == HISTORY_CAPACITY {
                self.fragments.pop_front();
            }
            self.fragments.push_back(state);
            
        }

        self.cursor = (self.cursor + 1).min(self.fragments.len());

        vm_result
    }

    pub(super) fn restore_external_state(&self, vm: &mut VM) {
        // restore state of vm that is independent of the vm step (input state)
        if self.redo_amount() > 0 {
            // in past
            self.fragments[self.cursor].restore(vm);
        } else if let Some(present_fragment) = &self.present_fragment {
            present_fragment.restore(vm);
        }
    }

    pub(super) fn handle_key_event(
        &self,
        event: KeyEvent,
        active: &mut bool,
        cursor_change: &mut (usize, bool),
    ) -> bool {
        let cursor = self.cursor;
        let mut new_cursor = self.cursor;
        match event.code {
            KeyCode::Esc => {
                *active = false;
            }
            KeyCode::Down | KeyCode::Char('s') | KeyCode::Char('S') => {
                new_cursor = self.cursor.saturating_add(1).min(self.fragments.len());
            }
            KeyCode::Up | KeyCode::Char('w') | KeyCode::Char('W') => {
                new_cursor = self.cursor.saturating_sub(1);
            }
            KeyCode::Home => {
                new_cursor = 0;
            }
            KeyCode::End => {
                new_cursor = self.fragments.len();
            }
            _ => return false,
        }
        (*cursor_change).0 = new_cursor.abs_diff(cursor);
        (*cursor_change).1 = new_cursor > cursor;
        true
    }
}

pub(super) struct HistoryWidget<'a> {
    pub(super) history: &'a History,
    pub(super) active: bool,
    pub(super) border: Borders,
}

impl<'a> Widget for HistoryWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.area() == 0 {
            return;
        }

        let history = &self.history.fragments;
        let cursor = self.history.cursor;

        let history_block = Block::default()
            .title(if cursor < history.len() {
                format!(" History ({}/{}) ", cursor + 1, history.len())
            } else {
                format!(" History ({}) ", history.len())
            })
            .borders(self.border);
        let history_inner_area = history_block.inner(area);

        let mut lbound = cursor.saturating_sub(history_inner_area.height as usize / 2);
        let mut rbound =
            cursor.saturating_add(history_inner_area.height as usize - (cursor - lbound));

        if rbound > history.len() + 1 {
            lbound = lbound.saturating_sub(rbound - (history.len() + 1));
            rbound = history.len() + 1;
        }

        let mut lines = Vec::with_capacity(rbound - lbound);

        if history_inner_area.area() > 0 {
            let mut asm = String::new();
            let mut asm_desc = String::new();

            for interp_state in history
                .range(lbound..rbound.min(history.len()))
                .map(|fragment| &fragment.interpreter)
            {
                asm.clear();
                asm_desc.clear();
                write!(&mut asm, "  {:#05X}: ", interp_state.pc).ok();
                asm_desc.push_str(ADDRESS_COMMENT_TOKEN);
                asm_desc.push(' ');
                if let Some(inst) = interp_state.instruction.as_ref() {
                    write_inst_dasm(inst, self.history.rom_config, &mut asm, &mut asm_desc).ok();
                } else {
                    asm.push_str("BAD INSTRUCTION");
                }

                if asm_desc.len() > ADDRESS_COMMENT_TOKEN.len() + 1 {
                    for _ in 0..(10 + INSTRUCTION_MAX_LENGTH).saturating_sub(asm.len()) {
                        asm.push(' ');
                    }

                    lines.push(Spans::from(vec![
                        Span::raw(asm.clone()),
                        Span::styled(asm_desc.clone(), Style::default().fg(Color::Yellow)),
                    ]));
                } else {
                    lines.push(Spans::from(asm.clone()));
                }
            }

            if rbound == history.len() + 1 {
                lines.push(Spans::from("  PRESENT"));
            }

            if let Some(line) = lines.get_mut(cursor - lbound) {
                let span_len = (history_inner_area.width as usize)
                    .saturating_sub(line.0.iter().fold(0, |len, span| len + span.width()));
                let content = line
                    .0
                    .last_mut()
                    .expect("Line should be nonempty")
                    .content
                    .to_mut();
                for _ in 0..span_len {
                    content.push(' ');
                }
                for span in line.0.iter_mut() {
                    span.style = Style::default()
                        .bg(if self.active {
                            Color::White
                        } else {
                            Color::LightGreen
                        })
                        .fg(Color::Black)
                        .add_modifier(Modifier::BOLD);
                }
            }
        }

        Paragraph::new(lines).block(history_block).render(area, buf);
    }
}
