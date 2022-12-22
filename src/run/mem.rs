pub const MEM_ACCESS_DRAW_FLAG: u8 = 0b1;
pub const MEM_ACCESS_READ_FLAG: u8 = 0b10;
pub const MEM_ACCESS_WRITE_FLAG: u8 = 0b100;
pub const MEM_ACCESS_EXEC_FLAG: u8 = 0b1000;

pub fn extract_access_flags(flag: u8) -> (bool, bool, bool, bool) {
    (
        flag & MEM_ACCESS_DRAW_FLAG == MEM_ACCESS_DRAW_FLAG,
        flag & MEM_ACCESS_READ_FLAG == MEM_ACCESS_READ_FLAG,
        flag & MEM_ACCESS_WRITE_FLAG == MEM_ACCESS_WRITE_FLAG,
        flag & MEM_ACCESS_EXEC_FLAG == MEM_ACCESS_EXEC_FLAG
    )
}