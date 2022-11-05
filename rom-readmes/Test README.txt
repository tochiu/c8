60FF F015 6000 
6900 6E00 6000 3001 3000 1392 7E01
6001 4001 4000 1392 7E01 
6101 6200 5020 5010 1392 7E01 
7002 70FF 3F00 1392 3002 1500 7E01  
6BF0 8CB0 5CB0 1392 7E01
6A0F 6BF0 8AB1 3AFF 1392 7E01
6A0F 6BF1 8AB2 3A01 1392 7E01
6A0F 6BF1 8AB3 3AFE 1392 7E01
6AFF 6BF1 8AB4 3AF0 1392 3F01 1392 7E01 6F00
6AFF 6BF1 8AB5 3A0E 1392 3F01 1392 7901 6E00 6F00
6A04 8A06 3A02 1300 3F00 1392 6A05 8A06 3A02 1300 3F01 1392 7E01
6AF0 6BC3 8AB7 3AD3 1392 3F00 1392 6AC3 6BF0 8AB7 3A2d 1392 3F01 1392 7E01
6A04 8A0E 3A08 1392 3F00 1392 6A84 8A0E 3A08 1392 3F01 1392 7E01
6A00 6B00 9AB0 5AB0 1392 6A01 9AB0 1392 7E01
6004 B2FC 1392 1392 1392 1392 1392 1392 7E01 
CA0F CBF0 8AB2 3A00 1392 CAFF CBFF 9AB0 1392 7E01
FA07 4AFF 1392 7E01
6AFF A500 FA33 F265 3002 1392 3105 1392 3205 1392 7E01
6000 6101 6202 6303 6404 F455 60FF 61FF 62FF 63FF 64FF F465 3000 1392 3404 1392 7E01
60FC A550 F055 A540 6010 F01E F065 30FC 1392 6E00 7901

3E00 1500 3902 1392
6A19 6B10 A388 DAB5
6A20 6B10 A38d DAB5 6F00 1386
60 90 90 90 60 90 a0 c0 a0 90
00E0 F929 6510 6419 D455 6F00 
FE29 6510 6420 D455 6F00 13a8

****************************************  
*                Made by               *   
*       https://github.com/Skosulor    *
****************************************


HOW TO:
  If a number is displayed you got an error with the corresponding instruction
  below. If an "OK" is displayed your interperator have passed the test. 

TEST ORDER
0: 3XNN
1: 4XNN
2: 5XY0
3: 7XNN (not carry flag and overflow value)
4: 8XY0
5: 8XY1
6: 8XY2
7: 8XY3
8: 8XY4
9: 8XY5
10: 8XY6
11: 8XY7
12: 8XYE
13: 9XY0
14: BNNN
15: CXNN  Note: Always a small chance of failure if(rand() == rand()) { fail }
16: FX07  Note: If fail it may be because either FX15 or FX07 fails or because delay_timer is 
                not implemented. If the the interplation is too fast this might also fail. 
17:FX33/FX65/ANNN
18:FX55/FX65
19: FX1E

create bin:
  xdd -r -p chip8_test.txt c8_test.c8

NOT TESTED: 0NNN
            00E0
            00EE
            1NNN
            2NNN
            6XNN (not tested but test wont work without this working)
            DXYN
            EX9E
            EXA1
            FX0A
            FX18
            FX29 (slightly tested with the result showing)
