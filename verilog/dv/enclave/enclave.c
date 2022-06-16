/*
 * SPDX-FileCopyrightText: 2020 Efabless Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * SPDX-License-Identifier: Apache-2.0
 */

// This include is relative to $CARAVEL_PATH (see Makefile)
#include <defs.h>
#include <stub.c>

#define reg_mprj_cfg_opcode (*(volatile uint32_t*)0x30000000)

#define BASE_WRITE_ADDR 0x30000004
#define END_WRITE_ADDR 0x30080000

#define OUT_ADDR_START 1048576
#define OP1_ADDR_START 4
#define OP2_ADDR_START 2048
#define OP_EN_BIT 2147483648

/*
	Wishbone Test:
		- Configures MPRJ lower 8-IO pins as outputs
		- Checks counter value through the wishbone port
*/

void write_to_chip(int addr, int data_in) {
	int wb_addr = (addr * 4) + BASE_WRITE_ADDR;
	if (wb_addr >= BASE_WRITE_ADDR && wb_addr < END_WRITE_ADDR){
		(*(volatile uint32_t*)wb_addr) = data_in;
	}
}

int read_from_chip(int addr) {
	int data_out = 0;
	int wb_addr = (addr * 4) + BASE_WRITE_ADDR;
        if (wb_addr >= BASE_WRITE_ADDR && wb_addr < END_WRITE_ADDR){
                data_out = (*(volatile uint32_t*)wb_addr);
		data_out = (*(volatile uint32_t*)wb_addr);
        } else {
		return 0;
	}
	return data_out;
}

void start_op(int out_addr, int op1_addr, int op2_addr, int op) {
	int op_instruction = op + (OP1_ADDR_START)*(op1_addr) + (OP2_ADDR_START)*(op2_addr) + (OUT_ADDR_START)*(out_addr) + OP_EN_BIT;
        reg_mprj_cfg_opcode = op_instruction;
}

void main()
{

	/*
	IO Control Registers
	| DM     | VTRIP | SLOW  | AN_POL | AN_SEL | AN_EN | MOD_SEL | INP_DIS | HOLDH | OEB_N | MGMT_EN |
	| 3-bits | 1-bit | 1-bit | 1-bit  | 1-bit  | 1-bit | 1-bit   | 1-bit   | 1-bit | 1-bit | 1-bit   |
	Output: 0000_0110_0000_1110  (0x1808) = GPIO_MODE_USER_STD_OUTPUT
	| DM     | VTRIP | SLOW  | AN_POL | AN_SEL | AN_EN | MOD_SEL | INP_DIS | HOLDH | OEB_N | MGMT_EN |
	| 110    | 0     | 0     | 0      | 0      | 0     | 0       | 1       | 0     | 0     | 0       |


	Input: 0000_0001_0000_1111 (0x0402) = GPIO_MODE_USER_STD_INPUT_NOPULL
	| DM     | VTRIP | SLOW  | AN_POL | AN_SEL | AN_EN | MOD_SEL | INP_DIS | HOLDH | OEB_N | MGMT_EN |
	| 001    | 0     | 0     | 0      | 0      | 0     | 0       | 0       | 0     | 1     | 0       |
	*/

	/* Set up the housekeeping SPI to be connected internally so	*/
	/* that external pin changes don't affect it.			*/

    reg_spi_enable = 1;
    reg_wb_enable = 1;
	// reg_spimaster_config = 0xa002;	// Enable, prescaler = 2,
                                        // connect to housekeeping SPI

	// Connect the housekeeping SPI to the SPI master
	// so that the CSB line is not left floating.  This allows
	// all of the GPIO pins to be used for user functions.
    // Monitor IO
    reg_mprj_io_37 = GPIO_MODE_USER_STD_OUTPUT;
    reg_mprj_io_36 = GPIO_MODE_USER_STD_OUTPUT;
    reg_mprj_io_35 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_34 = GPIO_MODE_USER_STD_INPUT_NOPULL;

    reg_mprj_io_33 = GPIO_MODE_USER_STD_OUTPUT;
    reg_mprj_io_32 = GPIO_MODE_USER_STD_OUTPUT;
    reg_mprj_io_31 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_30 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_29 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_28 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_27 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_26 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_25 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_24 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_23 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_22 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_21 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_20 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_19 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_18 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_17 = GPIO_MODE_MGMT_STD_OUTPUT;
    reg_mprj_io_16 = GPIO_MODE_MGMT_STD_OUTPUT;

    // IO outputs
    reg_mprj_io_15 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_14 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_13 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_12 = GPIO_MODE_USER_STD_INPUT_NOPULL; 
    reg_mprj_io_11 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_10 = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_9  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_8  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_7  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_6  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_5  = GPIO_MODE_USER_STD_INPUT_NOPULL;  
    reg_mprj_io_4  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_3  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_2  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_1  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    reg_mprj_io_0  = GPIO_MODE_USER_STD_INPUT_NOPULL;
    
    reg_mprj_xfer = 1;

    //Reset
    reg_la0_oenb = reg_la0_iena = 0x0000000F;    // [31:0]

    reg_la0_data = 0x00000000;
    reg_la0_data = 0x00000002;
    
    //Signal Start Test
    reg_mprj_datal = 0xAB600000;

    //Load Plain Text
    write_to_chip(0, 10);
    write_to_chip(100, 20);

    write_to_chip(1, 11);
    write_to_chip(101, 21);

    write_to_chip(2, 12);
    write_to_chip(102, 22);

    write_to_chip(3, 13);
    write_to_chip(103, 23);

    write_to_chip(4, 14);
    write_to_chip(104, 24);

    write_to_chip(5, 15);
    write_to_chip(105, 25);


    //Test Checker
    int fail = 0;
    unsigned int rtl;

    //Add
    start_op(50, 0, 100, 2);

    rtl = read_from_chip(50);
    if (rtl != 30) fail=1;

    rtl = read_from_chip(51);
    if (rtl != 32) fail=1;

    rtl = read_from_chip(52);
    if (rtl != 34) fail=1;


    //Decrypt
    start_op(30, 0, 100, 1);

    rtl = read_from_chip(30);
    if (rtl != 695) fail=1;

    
    //Multiply
    start_op(40, 0, 100, 3);

    rtl = read_from_chip(40);
    if (rtl != 200) fail=1;

    rtl = read_from_chip(41);
    if (rtl != 430) fail=1;

    rtl = read_from_chip(42);
    if (rtl != 691) fail=1;

    rtl = read_from_chip(43);
    if (rtl != 494) fail=1;

    rtl = read_from_chip(44);
    if (rtl != 264) fail=1;

    //Encrypt
    start_op(70, 0, 100, 0);

    rtl = read_from_chip(70);
    if (rtl != 62) fail=1;

    rtl = read_from_chip(71);
    if (rtl != 70) fail=1;

    rtl = read_from_chip(72);
    if (rtl != 78) fail=1;
    /**/

    //Check Pass
    if (fail == 0) {
        reg_mprj_datal = 0xAB610000;
    }
}

