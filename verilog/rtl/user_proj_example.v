module user_proj_example (
	vccd1,
	vssd1,
	la_data_in,
	la_data_out,
	la_oenb,
	io_in,
	io_out,
	io_oeb,
	user_irq,
	user_clock2,
	analog_io,
	wb_clk_i,
	wb_rst_i,
	wbs_stb_i,
	wbs_cyc_i,
	wbs_we_i,
	wbs_sel_i,
	wbs_dat_i,
	wbs_adr_i,
	wbs_ack_o,
	wbs_dat_o
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 16;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 32;
	parameter DIMENSION = 2;
	parameter BIG_N = 3;
	parameter OPCODE_ADDR = 32'h30000000;
	parameter OUTPUT_ADDR = 32'h00000001;
	parameter DATA_WIDTH = 128;
	parameter ADDR_WIDTH = 9;
	parameter DEPTH = 256;
	parameter DIM_WIDTH = 8;
	parameter PARALLEL = 1;
	parameter USE_POWER_PINS = 0;
	parameter ENABLE_FULL_IO = 0;
	inout vccd1;
	inout vssd1;
	input wire [127:0] la_data_in;
	output wire [127:0] la_data_out;
	input wire [127:0] la_oenb;
	input wire [37:0] io_in;
	output wire [37:0] io_out;
	output wire [37:0] io_oeb;
	output wire [2:0] user_irq;
	input user_clock2;
	inout [28:0] analog_io;
	input wire wb_clk_i;
	input wire wb_rst_i;
	input wire wbs_stb_i;
	input wire wbs_cyc_i;
	input wire wbs_we_i;
	input wire [3:0] wbs_sel_i;
	input wire [31:0] wbs_dat_i;
	input wire [31:0] wbs_adr_i;
	output wire wbs_ack_o;
	output wire [31:0] wbs_dat_o;
	wire clk;
	wire rst_n;
	assign user_irq = 3'b000;
	assign la_data_out = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
	assign io_out = 0;
	assign io_oeb = 0;
	assign clk = wb_clk_i;
	assign rst_n = (~la_oenb[1] ? la_data_in[1] : 0);
	wire [31:0] wishbone_output;
	wire [31:0] wishbone_data;
	wire [31:0] wishbone_addr;
	wire wb_read_req;
	wire wb_write_req;
	wire config_en;
	wire valid_opcode;
	wire [1:0] opcode;
	wire [ADDR_WIDTH - 1:0] op1_base_addr;
	wire [ADDR_WIDTH - 1:0] op2_base_addr;
	wire [ADDR_WIDTH - 1:0] out_base_addr;
	wire [1:0] opcode_out;
	wire [ADDR_WIDTH - 1:0] op1_addr;
	wire [ADDR_WIDTH - 1:0] op2_addr;
	wire [ADDR_WIDTH - 1:0] out_addr;
	wire op_select;
	reg op_select_delayed;
	wire en;
	wire done;
	wire [DIM_WIDTH:0] row;
	wire in_wen;
	reg delayed_in_wen;
	wire [ADDR_WIDTH - 1:0] in_wadr;
	wire [DATA_WIDTH - 1:0] in_wdata;
	reg out_wen;
	reg [ADDR_WIDTH - 1:0] out_wadr_delay_stage;
	reg [ADDR_WIDTH - 1:0] out_wadr;
	wire [DATA_WIDTH - 1:0] out_wdata;
	wire op1_ren;
	wire [ADDR_WIDTH - 1:0] op1_radr;
	wire [DATA_WIDTH - 1:0] op1_rdata;
	wire op2_ren;
	wire [ADDR_WIDTH - 1:0] op2_radr;
	wire [DATA_WIDTH - 1:0] op2_rdata;
	wire out_ren;
	reg delayed_out_ren;
	wire [ADDR_WIDTH - 1:0] out_radr;
	wire [DATA_WIDTH - 1:0] out_rdata;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] op1_structured;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] op2_structured;
	reg [DIM_WIDTH:0] delayed_row;
	wire [CIPHERTEXT_WIDTH - 1:0] encrypt_out;
	reg encrypt_en;
	wire [PLAINTEXT_WIDTH - 1:0] decrypt_out;
	wire decrypt_en;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] add_out;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] add_out_flattened;
	wire add_en;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] muxed_ops;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] mult_out;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] mult_out_flattened;
	wire mult_en;
	assign wishbone_output = out_rdata;
	wishbone_ctl #(.OPCODE_ADDR(OPCODE_ADDR)) wb_inst(
		.wb_clk_i(wb_clk_i),
		.wb_rst_i(wb_rst_i),
		.wbs_stb_i(wbs_stb_i),
		.wbs_cyc_i(wbs_cyc_i),
		.wbs_we_i(wbs_we_i),
		.wbs_sel_i(wbs_sel_i),
		.wbs_dat_i(wbs_dat_i),
		.wbs_adr_i(wbs_adr_i),
		.wishbone_output(wishbone_output),
		.config_en(config_en),
		.wishbone_data(wishbone_data),
		.wishbone_addr(wishbone_addr),
		.wb_read_req(wb_read_req),
		.wb_write_req(wb_write_req),
		.wbs_ack_o(wbs_ack_o),
		.wbs_dat_o(wbs_dat_o)
	);
	assign opcode = wishbone_data[1:0];
	assign op1_base_addr = wishbone_data[(2 + ADDR_WIDTH) - 1:2];
	assign op2_base_addr = wishbone_data[(2 + (2 * ADDR_WIDTH)) - 1:2 + ADDR_WIDTH];
	assign out_base_addr = wishbone_data[(2 + (3 * ADDR_WIDTH)) - 1:2 + (2 * ADDR_WIDTH)];
	assign valid_opcode = wishbone_data[31];
	controller #(
		.PLAINTEXT_MODULUS(PLAINTEXT_MODULUS),
		.PLAINTEXT_WIDTH(PLAINTEXT_WIDTH),
		.CIPHERTEXT_MODULUS(CIPHERTEXT_MODULUS),
		.CIPHERTEXT_WIDTH(CIPHERTEXT_WIDTH),
		.DIMENSION(DIMENSION),
		.DIM_WIDTH(DIM_WIDTH),
		.BIG_N(BIG_N),
		.ADDR_WIDTH(ADDR_WIDTH),
		.PARALLEL(PARALLEL)
	) controller_inst(
		.clk(clk),
		.rst_n(rst_n),
		.opcode(opcode),
		.config_en(config_en && valid_opcode),
		.op1_base_addr(op1_base_addr),
		.op2_base_addr(op2_base_addr),
		.out_base_addr(out_base_addr),
		.opcode_out(opcode_out),
		.op1_addr(op1_addr),
		.op2_addr(op2_addr),
		.out_addr(out_addr),
		.op_select(op_select),
		.en(en),
		.done(done),
		.row(row)
	);
	always @(posedge clk) begin
		delayed_in_wen = wb_write_req & !config_en;
		delayed_out_ren <= wb_read_req;
	end
	assign in_wen = delayed_in_wen;
	assign in_wadr = wishbone_addr[ADDR_WIDTH:0];
	assign in_wdata = wishbone_data;
	assign out_wdata = (opcode_out == 2'b00 ? encrypt_out : (opcode_out == 2'b01 ? decrypt_out : (opcode_out == 2'b10 ? add_out_flattened : mult_out_flattened)));
	assign op1_ren = en;
	assign op1_radr = op1_addr;
	assign op2_ren = en;
	assign op2_radr = op2_addr;
	assign out_ren = wb_read_req;
	assign out_radr = wishbone_addr[ADDR_WIDTH:0];
	sram #(
		.DATA_WIDTH(DATA_WIDTH),
		.ADDR_WIDTH(ADDR_WIDTH),
		.DEPTH(DEPTH)
	) sram_inst(
		.clk(clk),
		.in_wen(in_wen),
		.in_wadr(in_wadr),
		.in_wdata(in_wdata),
		.out_wen(out_wen),
		.out_wadr(out_wadr),
		.out_wdata(out_wdata),
		.op1_ren(op1_ren),
		.op1_radr(op1_radr),
		.op1_rdata(op1_rdata),
		.op2_ren(op2_ren),
		.op2_radr(op2_radr),
		.op2_rdata(op2_rdata),
		.out_ren(out_ren),
		.out_radr(out_radr),
		.out_rdata(out_rdata)
	);
	genvar ip;
	generate
		for (ip = 0; ip < PARALLEL; ip = ip + 1) begin : genblk1
			assign op1_structured[ip * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = op1_rdata[((ip + 1) * CIPHERTEXT_WIDTH) - 1:ip * CIPHERTEXT_WIDTH];
			assign op2_structured[ip * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = op2_rdata[((ip + 1) * CIPHERTEXT_WIDTH) - 1:ip * CIPHERTEXT_WIDTH];
		end
	endgenerate
	always @(posedge clk) begin
		delayed_row <= row;
		out_wen <= en;
		out_wadr_delay_stage <= out_addr;
		out_wadr <= out_wadr_delay_stage;
		op_select_delayed <= op_select;
		encrypt_en <= (opcode_out == 2'b00) && en;
	end
	encrypt #(
		.PLAINTEXT_MODULUS(PLAINTEXT_MODULUS),
		.PLAINTEXT_WIDTH(PLAINTEXT_WIDTH),
		.CIPHERTEXT_MODULUS(CIPHERTEXT_MODULUS),
		.CIPHERTEXT_WIDTH(CIPHERTEXT_WIDTH),
		.DIMENSION(DIMENSION),
		.DIM_WIDTH(DIM_WIDTH),
		.BIG_N(BIG_N),
		.PARALLEL(PARALLEL)
	) encrypt_inst(
		.clk(clk),
		.rst_n(rst_n),
		.en(encrypt_en),
		.done(done),
		.op1(op1_structured),
		.op2(op2_structured),
		.row(delayed_row),
		.ciphertext(encrypt_out)
	);
	assign decrypt_en = (opcode_out == 2'b01) && en;
	decrypt #(
		.PLAINTEXT_MODULUS(PLAINTEXT_MODULUS),
		.PLAINTEXT_WIDTH(PLAINTEXT_WIDTH),
		.CIPHERTEXT_MODULUS(CIPHERTEXT_MODULUS),
		.CIPHERTEXT_WIDTH(CIPHERTEXT_WIDTH),
		.DIMENSION(DIMENSION),
		.DIM_WIDTH(DIM_WIDTH),
		.BIG_N(BIG_N),
		.PARALLEL(PARALLEL)
	) decrypt_inst(
		.clk(clk),
		.rst_n(rst_n),
		.en(en),
		.secretkey_entry(op1_structured),
		.ciphertext_entry(op2_structured),
		.row(delayed_row),
		.result(decrypt_out)
	);
	assign add_en = (opcode_out == 2'b10) && en;
	genvar iaf;
	generate
		for (iaf = 0; iaf < PARALLEL; iaf = iaf + 1) begin : genblk2
			assign add_out_flattened[((iaf + 1) * CIPHERTEXT_WIDTH) - 1:iaf * CIPHERTEXT_WIDTH] = add_out[iaf * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		end
	endgenerate
	homomorphic_add #(
		.PLAINTEXT_MODULUS(PLAINTEXT_MODULUS),
		.PLAINTEXT_WIDTH(PLAINTEXT_WIDTH),
		.CIPHERTEXT_MODULUS(CIPHERTEXT_MODULUS),
		.CIPHERTEXT_WIDTH(CIPHERTEXT_WIDTH),
		.DIMENSION(DIMENSION),
		.BIG_N(BIG_N),
		.PARALLEL(PARALLEL)
	) homomorphic_inst(
		.clk(clk),
		.rst_n(rst_n),
		.en(en),
		.ciphertext1(op1_structured),
		.ciphertext2(op2_structured),
		.result(add_out)
	);
	assign mult_en = (opcode_out == 2'b11) && en;
	genvar imf;
	generate
		for (imf = 0; imf < PARALLEL; imf = imf + 1) begin : genblk3
			assign mult_out_flattened[((imf + 1) * CIPHERTEXT_WIDTH) - 1:imf * CIPHERTEXT_WIDTH] = mult_out[imf * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		end
	endgenerate
	assign muxed_ops = (op_select_delayed == 0 ? op1_structured : op2_structured);
	homomorphic_multiply #(
		.PLAINTEXT_MODULUS(PLAINTEXT_MODULUS),
		.PLAINTEXT_WIDTH(PLAINTEXT_WIDTH),
		.CIPHERTEXT_MODULUS(CIPHERTEXT_MODULUS),
		.CIPHERTEXT_WIDTH(CIPHERTEXT_WIDTH),
		.DIMENSION(DIMENSION),
		.DIM_WIDTH(DIM_WIDTH),
		.BIG_N(BIG_N),
		.PARALLEL(PARALLEL)
	) homomorphic_multiply_inst(
		.clk(clk),
		.rst_n(rst_n),
		.op1(muxed_ops),
		.row(delayed_row),
		.ciphertext_select(op_select_delayed),
		.en(en),
		.result_partial(mult_out)
	);
endmodule
module sram (
	clk,
	in_wen,
	in_wadr,
	in_wdata,
	out_wen,
	out_wadr,
	out_wdata,
	op1_ren,
	op1_radr,
	op1_rdata,
	op2_ren,
	op2_radr,
	op2_rdata,
	out_ren,
	out_radr,
	out_rdata
);
	parameter DATA_WIDTH = 32;
	parameter ADDR_WIDTH = 10;
	parameter DEPTH = 1024;
	input clk;
	input in_wen;
	input [ADDR_WIDTH - 1:0] in_wadr;
	input [DATA_WIDTH - 1:0] in_wdata;
	input out_wen;
	input [ADDR_WIDTH - 1:0] out_wadr;
	input [DATA_WIDTH - 1:0] out_wdata;
	input op1_ren;
	input [ADDR_WIDTH - 1:0] op1_radr;
	output wire [DATA_WIDTH - 1:0] op1_rdata;
	input op2_ren;
	input [ADDR_WIDTH - 1:0] op2_radr;
	output wire [DATA_WIDTH - 1:0] op2_rdata;
	input out_ren;
	input [ADDR_WIDTH - 1:0] out_radr;
	output wire [DATA_WIDTH - 1:0] out_rdata;
	genvar x;
	genvar y;
	wire [DATA_WIDTH - 1:0] op1_rdata_w [(DEPTH / 256) - 1:0];
	reg [ADDR_WIDTH - 1:0] op1_radr_r;
	wire [DATA_WIDTH - 1:0] op2_rdata_w [(DEPTH / 256) - 1:0];
	reg [ADDR_WIDTH - 1:0] op2_radr_r;
	wire [DATA_WIDTH - 1:0] out_rdata_w [(DEPTH / 256) - 1:0];
	reg [ADDR_WIDTH - 1:0] out_radr_r;
	always @(posedge clk) begin
		op1_radr_r <= op1_radr;
		op2_radr_r <= op2_radr;
		out_radr_r <= out_radr;
	end
	generate
		for (x = 0; x < (DATA_WIDTH / 32); x = x + 1) begin : width_macro1
			for (y = 0; y < (DEPTH / 256); y = y + 1) begin : depth_macro1
				sky130_sram_1kbyte_1rw1r_32x256_8 #(.VERBOSE(0)) sram1(
					.clk0(clk),
					.csb0(~(in_wen && (in_wadr[ADDR_WIDTH - 1:8] == y))),
					.web0(~(in_wen && (in_wadr[ADDR_WIDTH - 1:8] == y))),
					.wmask0(4'hf),
					.addr0(in_wadr[7:0]),
					.din0(in_wdata[(32 * (x + 1)) - 1:32 * x]),
					.clk1(clk),
					.csb1(~(op1_ren && (op1_radr[ADDR_WIDTH - 1:8] == y))),
					.addr1(op1_radr[7:0]),
					.dout1(op1_rdata_w[y][(32 * (x + 1)) - 1:32 * x])
				);
			end
		end
		for (x = 0; x < (DATA_WIDTH / 32); x = x + 1) begin : width_macro2
			for (y = 0; y < (DEPTH / 256); y = y + 1) begin : depth_macro2
				sky130_sram_1kbyte_1rw1r_32x256_8 #(.VERBOSE(0)) sram2(
					.clk0(clk),
					.csb0(~(in_wen && (in_wadr[ADDR_WIDTH - 1:8] == y))),
					.web0(~(in_wen && (in_wadr[ADDR_WIDTH - 1:8] == y))),
					.wmask0(4'hf),
					.addr0(in_wadr[7:0]),
					.din0(in_wdata[(32 * (x + 1)) - 1:32 * x]),
					.clk1(clk),
					.csb1(~(op2_ren && (op2_radr[ADDR_WIDTH - 1:8] == y))),
					.addr1(op2_radr[7:0]),
					.dout1(op2_rdata_w[y][(32 * (x + 1)) - 1:32 * x])
				);
			end
		end
		for (x = 0; x < (DATA_WIDTH / 32); x = x + 1) begin : width_macro3
			for (y = 0; y < (DEPTH / 256); y = y + 1) begin : depth_macro3
				sky130_sram_1kbyte_1rw1r_32x256_8 #(.VERBOSE(0)) sram3(
					.clk0(clk),
					.csb0(~(out_wen && (out_wadr[ADDR_WIDTH - 1:8] == y))),
					.web0(~(out_wen && (out_wadr[ADDR_WIDTH - 1:8] == y))),
					.wmask0(4'hf),
					.addr0(out_wadr[7:0]),
					.din0(out_wdata[(32 * (x + 1)) - 1:32 * x]),
					.clk1(clk),
					.csb1(~(out_ren && (out_radr[ADDR_WIDTH - 1:8] == y))),
					.addr1(out_radr[7:0]),
					.dout1(out_rdata_w[y][(32 * (x + 1)) - 1:32 * x])
				);
			end
		end
	endgenerate
	assign op1_rdata = op1_rdata_w[op1_radr_r[ADDR_WIDTH - 1:8]];
	assign op2_rdata = op2_rdata_w[op2_radr_r[ADDR_WIDTH - 1:8]];
	assign out_rdata = out_rdata_w[out_radr_r[ADDR_WIDTH - 1:8]];
endmodule
module wishbone_ctl (
	wb_clk_i,
	wb_rst_i,
	wbs_stb_i,
	wbs_cyc_i,
	wbs_we_i,
	wbs_sel_i,
	wbs_dat_i,
	wbs_adr_i,
	wishbone_output,
	config_en,
	wishbone_data,
	wishbone_addr,
	wb_read_req,
	wb_write_req,
	wbs_ack_o,
	wbs_dat_o
);
	parameter OPCODE_ADDR = 32'h30000000;
	input wb_clk_i;
	input wb_rst_i;
	input wbs_stb_i;
	input wbs_cyc_i;
	input wbs_we_i;
	input [3:0] wbs_sel_i;
	input [31:0] wbs_dat_i;
	input [31:0] wbs_adr_i;
	input [31:0] wishbone_output;
	output wire config_en;
	output wire [31:0] wishbone_data;
	output wire [31:0] wishbone_addr;
	output wire wb_read_req;
	output wire wb_write_req;
	output wire wbs_ack_o;
	output wire [31:0] wbs_dat_o;
	reg [31:0] wbs_reg_i;
	reg [31:0] wbs_reg_o;
	reg delayed_read_req;
	reg dd_read_req;
	reg [31:0] wbs_reg_addr;
	wire wbs_req = wbs_stb_i & wbs_cyc_i;
	reg ack_o;
	always @(posedge wb_clk_i)
		if (wb_rst_i)
			ack_o <= 1'b0;
		else
			ack_o <= wbs_req;
	wire wbs_req_write = (~ack_o & wbs_req) & wbs_we_i;
	wire wbs_req_read = (~ack_o & wbs_req) & ~wbs_we_i;
	always @(posedge wb_clk_i) begin
		wbs_reg_addr = wbs_adr_i;
		if (wb_rst_i)
			wbs_reg_i <= 32'd0;
		else if (wbs_req_write)
			wbs_reg_i <= wbs_dat_i;
	end
	always @(posedge wb_clk_i) begin
		dd_read_req = delayed_read_req;
		delayed_read_req = wbs_req_read;
		if (wb_rst_i)
			wbs_reg_o <= 32'd0;
		else if (dd_read_req)
			wbs_reg_o <= wishbone_output;
	end
	assign config_en = wbs_req & (wbs_adr_i == OPCODE_ADDR);
	assign wbs_ack_o = ack_o;
	assign wbs_dat_o = wbs_reg_o;
	assign wishbone_data = wbs_dat_i;
	assign wishbone_addr = (wbs_adr_i - 32'h30000004) >> 2;
	assign wb_read_req = wbs_req_read;
	assign wb_write_req = wbs_req_write;
endmodule
module controller (
	clk,
	rst_n,
	opcode,
	config_en,
	op1_base_addr,
	op2_base_addr,
	out_base_addr,
	opcode_out,
	op1_addr,
	op2_addr,
	out_addr,
	op_select,
	en,
	done,
	row
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 6;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 10;
	parameter DIMENSION = 10;
	parameter BIG_N = 30;
	parameter DIM_WIDTH = 4;
	parameter ADDR_WIDTH = 10;
	parameter PARALLEL = 1;
	input clk;
	input rst_n;
	input [1:0] opcode;
	input config_en;
	input [ADDR_WIDTH - 1:0] op1_base_addr;
	input [ADDR_WIDTH - 1:0] op2_base_addr;
	input [ADDR_WIDTH - 1:0] out_base_addr;
	output wire [1:0] opcode_out;
	output wire [ADDR_WIDTH - 1:0] op1_addr;
	output wire [ADDR_WIDTH - 1:0] op2_addr;
	output wire [ADDR_WIDTH - 1:0] out_addr;
	output wire op_select;
	output wire en;
	output wire done;
	output wire [DIM_WIDTH:0] row;
	reg [ADDR_WIDTH - 1:0] op1_base_addr_stored;
	reg [ADDR_WIDTH - 1:0] op2_base_addr_stored;
	reg [ADDR_WIDTH - 1:0] out_base_addr_stored;
	reg [DIM_WIDTH - 1:0] col;
	reg [1:0] opcode_out_reg;
	reg [ADDR_WIDTH - 1:0] op1_addr_reg;
	reg [ADDR_WIDTH - 1:0] op2_addr_reg;
	reg [ADDR_WIDTH - 1:0] out_addr_reg;
	reg op_select_reg;
	reg en_reg;
	reg done_reg;
	reg [DIM_WIDTH:0] row_reg;
	always @(posedge clk) begin
		if (en_reg)
			case (opcode_out_reg)
				2'b00:
					if ((row_reg > DIMENSION) && (col > 0)) begin
						en_reg <= 0;
						done_reg <= 1;
						row_reg <= 0;
						col <= 0;
						out_addr_reg <= out_addr_reg + 1;
					end
					else if ((row_reg != 0) && (col == 0)) begin
						op1_addr_reg <= op1_addr_reg + 1;
						op2_addr_reg <= op2_addr_reg + 1;
						out_addr_reg <= out_addr_reg + 1;
						col <= col + 2;
					end
					else if ((col + 1) < BIG_N) begin
						op1_addr_reg <= op1_addr_reg + 1;
						op2_addr_reg <= op2_addr_reg + 1;
						col <= col + 2;
					end
					else if ((col + 1) >= BIG_N) begin
						op1_addr_reg <= op1_addr_reg + 1;
						op2_addr_reg <= op2_addr_reg + 1;
						row_reg <= row_reg + 1;
						col <= 0;
					end
					else begin
						en_reg <= 0;
						done_reg <= 1;
					end
				2'b01:
					if (op1_addr_reg <= (op1_base_addr_stored + (DIMENSION / PARALLEL))) begin
						op1_addr_reg <= op1_addr_reg + 1;
						op2_addr_reg <= op2_addr_reg + 1;
						row_reg <= row_reg + 1;
					end
					else begin
						en_reg <= 0;
						done_reg <= 1;
					end
				2'b10:
					if (op1_addr_reg <= (op1_base_addr_stored + (DIMENSION / PARALLEL))) begin
						op1_addr_reg <= op1_addr_reg + 1;
						op2_addr_reg <= op2_addr_reg + 1;
						out_addr_reg <= out_addr_reg + 1;
					end
					else begin
						en_reg <= 0;
						done_reg <= 1;
					end
				2'b11:
					if (op1_addr_reg < (op1_base_addr_stored + (DIMENSION / PARALLEL))) begin
						op1_addr_reg <= op1_addr_reg + 1;
						row_reg <= row_reg + 1;
					end
					else if ((op_select_reg != 1) && !done_reg) begin
						row_reg <= 0;
						op_select_reg <= 1;
					end
					else if (op2_addr_reg < (op2_base_addr_stored + (DIMENSION / PARALLEL))) begin
						op2_addr_reg <= op2_addr_reg + 1;
						out_addr_reg <= out_addr_reg + 1;
						row_reg <= row_reg + 1;
						op_select_reg <= 1;
					end
					else if (op2_addr_reg == (op2_base_addr_stored + (DIMENSION / PARALLEL))) begin
						op2_addr_reg <= op2_addr_reg + 1;
						out_addr_reg <= out_addr_reg + 1;
						row_reg <= row_reg + 1;
						op_select_reg <= 0;
						done_reg <= 1;
					end
					else if (row_reg <= ((2 * DIMENSION) / PARALLEL)) begin
						out_addr_reg <= out_addr_reg + 1;
						row_reg <= row_reg + 1;
					end
					else begin
						en_reg <= 0;
						done_reg <= 1;
					end
			endcase
		if ((!en_reg && !done_reg) && !config_en)
			en_reg <= 1;
		if (config_en) begin
			opcode_out_reg <= opcode;
			op1_addr_reg <= op1_base_addr;
			op2_addr_reg <= op2_base_addr;
			out_addr_reg <= out_base_addr;
			op1_base_addr_stored <= op1_base_addr;
			op2_base_addr_stored <= op2_base_addr;
			out_base_addr_stored <= out_base_addr;
			en_reg <= 0;
			op_select_reg <= 0;
			done_reg <= 0;
			row_reg <= 0;
			col <= 0;
		end
		if (!rst_n) begin
			opcode_out_reg <= 0;
			op1_addr_reg <= 0;
			op2_addr_reg <= 0;
			out_addr_reg <= 0;
			op1_base_addr_stored <= 0;
			op2_base_addr_stored <= 0;
			out_base_addr_stored <= 0;
			op_select_reg <= 0;
			en_reg <= 0;
			row_reg <= 0;
			col <= 0;
		end
	end
	assign opcode_out = opcode_out_reg;
	assign op1_addr = op1_addr_reg;
	assign op2_addr = op2_addr_reg;
	assign out_addr = out_addr_reg;
	assign op_select = op_select_reg;
	assign en = en_reg;
	assign done = done_reg;
	assign row = row_reg;
endmodule
module encrypt (
	clk,
	rst_n,
	en,
	done,
	op1,
	op2,
	row,
	ciphertext
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 6;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 32;
	parameter DIMENSION = 128;
	parameter DIM_WIDTH = 7;
	parameter BIG_N = 30;
	parameter PARALLEL = 2;
	input clk;
	input rst_n;
	input en;
	input done;
	input [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] op1;
	input [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] op2;
	input [DIM_WIDTH:0] row;
	output reg [CIPHERTEXT_WIDTH - 1:0] ciphertext;
	reg [(DIMENSION >= 0 ? ((DIMENSION + 1) * CIPHERTEXT_WIDTH) - 1 : ((1 - DIMENSION) * CIPHERTEXT_WIDTH) + ((DIMENSION * CIPHERTEXT_WIDTH) - 1)):(DIMENSION >= 0 ? 0 : DIMENSION * CIPHERTEXT_WIDTH)] psum;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] parallel1;
	wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] parallel2;
	reg [DIM_WIDTH - 1:0] last_row;
	genvar ienc;
	assign parallel1[0+:CIPHERTEXT_WIDTH] = (op1[CIPHERTEXT_WIDTH - 1] ? 0 : op1[0+:CIPHERTEXT_WIDTH]);
	assign parallel2[0+:CIPHERTEXT_WIDTH] = (op2[CIPHERTEXT_WIDTH - 1] ? 0 : op2[0+:CIPHERTEXT_WIDTH]);
	generate
		for (ienc = 1; ienc < PARALLEL; ienc = ienc + 1) begin : genblk1
			assign parallel1[ienc * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = parallel1[(ienc - 1) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] + (op1[(ienc * CIPHERTEXT_WIDTH) + (CIPHERTEXT_WIDTH - 1)] ? 0 : op1[ienc * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH]);
			assign parallel2[ienc * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = parallel2[(ienc - 1) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] + (op2[(ienc * CIPHERTEXT_WIDTH) + (CIPHERTEXT_WIDTH - 1)] ? 0 : op2[ienc * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH]);
		end
	endgenerate
	always @(posedge clk) begin
		if (en & (rst_n != 0))
			psum[(DIMENSION >= 0 ? row : DIMENSION - row) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] <= (psum[(DIMENSION >= 0 ? row : DIMENSION - row) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] + parallel1[(PARALLEL - 1) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH]) + parallel2[(PARALLEL - 1) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		else begin
			last_row <= 0;
			psum <= 0;
		end
		if (!rst_n)
			ciphertext <= 0;
		else if (row != last_row)
			ciphertext <= psum[(DIMENSION >= 0 ? last_row : DIMENSION - last_row) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		last_row <= row;
	end
endmodule
module decrypt (
	clk,
	rst_n,
	en,
	secretkey_entry,
	ciphertext_entry,
	row,
	result
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 6;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 10;
	parameter DIMENSION = 10;
	parameter DIM_WIDTH = 4;
	parameter BIG_N = 30;
	parameter PARALLEL = 1;
	input clk;
	input rst_n;
	input en;
	input [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] secretkey_entry;
	input [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] ciphertext_entry;
	input [DIM_WIDTH:0] row;
	output wire [PLAINTEXT_WIDTH - 1:0] result;
	wire [((2 * CIPHERTEXT_WIDTH) >= 0 ? (PARALLEL * ((2 * CIPHERTEXT_WIDTH) + 1)) - 1 : (PARALLEL * (1 - (2 * CIPHERTEXT_WIDTH))) + ((2 * CIPHERTEXT_WIDTH) - 1)):((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH)] parallel_accum;
	reg [2 * CIPHERTEXT_WIDTH:0] dot_product;
	genvar idec;
	assign parallel_accum[((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH)+:((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH))] = secretkey_entry[0+:CIPHERTEXT_WIDTH] * ciphertext_entry[0+:CIPHERTEXT_WIDTH];
	generate
		for (idec = 1; idec < PARALLEL; idec = idec + 1) begin : genblk1
			assign parallel_accum[((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH) + (idec * ((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH)))+:((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH))] = parallel_accum[((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH) + ((idec - 1) * ((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH)))+:((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH))] + (secretkey_entry[idec * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] * ciphertext_entry[idec * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH]);
		end
	endgenerate
	always @(posedge clk) begin
		if (en)
			if (row == 0)
				dot_product <= parallel_accum[((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH) + ((PARALLEL - 1) * ((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH)))+:((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH))];
			else
				dot_product <= dot_product + parallel_accum[((2 * CIPHERTEXT_WIDTH) >= 0 ? 0 : 2 * CIPHERTEXT_WIDTH) + ((PARALLEL - 1) * ((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH)))+:((2 * CIPHERTEXT_WIDTH) >= 0 ? (2 * CIPHERTEXT_WIDTH) + 1 : 1 - (2 * CIPHERTEXT_WIDTH))];
		if (!rst_n)
			dot_product <= 0;
	end
	assign result = dot_product[PLAINTEXT_WIDTH - 1:0];
endmodule
module homomorphic_add (
	clk,
	rst_n,
	en,
	ciphertext1,
	ciphertext2,
	result
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 6;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 10;
	parameter DIMENSION = 1;
	parameter BIG_N = 30;
	parameter PARALLEL = 1;
	input clk;
	input rst_n;
	input en;
	input signed [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] ciphertext1;
	input signed [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] ciphertext2;
	output wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] result;
	reg [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] ir;
	always @(posedge clk)
		if (en) begin : sv2v_autoblock_1
			reg signed [31:0] aaa;
			for (aaa = 0; aaa < PARALLEL; aaa = aaa + 1)
				ir[aaa * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] <= ciphertext1[aaa * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] + ciphertext2[aaa * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		end
		else
			ir <= 0;
	genvar iadd;
	generate
		for (iadd = 0; iadd < PARALLEL; iadd = iadd + 1) begin : genblk1
			assign result[iadd * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = ir[iadd * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		end
	endgenerate
endmodule
module homomorphic_multiply (
	clk,
	rst_n,
	op1,
	row,
	ciphertext_select,
	en,
	result_partial
);
	parameter PLAINTEXT_MODULUS = 64;
	parameter PLAINTEXT_WIDTH = 6;
	parameter DIMENSION = 1;
	parameter DIM_WIDTH = 1;
	parameter CIPHERTEXT_MODULUS = 1024;
	parameter CIPHERTEXT_WIDTH = 10;
	parameter BIG_N = 30;
	parameter PARALLEL = 1;
	input clk;
	input rst_n;
	input [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] op1;
	input [DIM_WIDTH:0] row;
	input ciphertext_select;
	input en;
	output wire [(PARALLEL * CIPHERTEXT_WIDTH) - 1:0] result_partial;
	reg [(DIMENSION >= 0 ? ((DIMENSION + 1) * CIPHERTEXT_WIDTH) - 1 : ((1 - DIMENSION) * CIPHERTEXT_WIDTH) + ((DIMENSION * CIPHERTEXT_WIDTH) - 1)):(DIMENSION >= 0 ? 0 : DIMENSION * CIPHERTEXT_WIDTH)] ciphertext1;
	reg [((2 * DIMENSION) >= 0 ? (((2 * DIMENSION) + 1) * CIPHERTEXT_WIDTH) - 1 : ((1 - (2 * DIMENSION)) * CIPHERTEXT_WIDTH) + (((2 * DIMENSION) * CIPHERTEXT_WIDTH) - 1)):((2 * DIMENSION) >= 0 ? 0 : (2 * DIMENSION) * CIPHERTEXT_WIDTH)] interim_result;
	wire [DIM_WIDTH:0] out_row;
	assign out_row = (row == 0 ? 0 : row - 1);
	always @(posedge clk)
		if (!rst_n) begin
			ciphertext1 <= 0;
			interim_result <= 0;
		end
		else if ((ciphertext_select == 0) && en) begin
			if (row <= DIMENSION) begin : sv2v_autoblock_1
				reg signed [31:0] jm;
				for (jm = 0; jm < PARALLEL; jm = jm + 1)
					ciphertext1[(DIMENSION >= 0 ? row + jm : DIMENSION - (row + jm)) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] <= op1[jm * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
			end
			else begin : sv2v_autoblock_2
				reg signed [31:0] km;
				for (km = 0; km < PARALLEL; km = km + 1)
					ciphertext1[(DIMENSION >= 0 ? (row + km) - DIMENSION : DIMENSION - ((row + km) - DIMENSION)) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] <= op1[km * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
			end
		end
		else if ((ciphertext_select == 1) && en) begin : sv2v_autoblock_3
			reg signed [31:0] xm;
			for (xm = 0; xm < PARALLEL; xm = xm + 1)
				begin : sv2v_autoblock_4
					reg signed [31:0] ym;
					for (ym = 0; ym <= DIMENSION; ym = ym + 1)
						interim_result[((2 * DIMENSION) >= 0 ? (row + xm) + ym : (2 * DIMENSION) - ((row + xm) + ym)) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] <= interim_result[((2 * DIMENSION) >= 0 ? (row + xm) + ym : (2 * DIMENSION) - ((row + xm) + ym)) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] + (op1[xm * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] * ciphertext1[(DIMENSION >= 0 ? ym : DIMENSION - ym) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH]);
				end
		end
	genvar mmm;
	generate
		for (mmm = 0; mmm < PARALLEL; mmm = mmm + 1) begin : genblk1
			assign result_partial[mmm * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH] = interim_result[((2 * DIMENSION) >= 0 ? out_row + mmm : (2 * DIMENSION) - (out_row + mmm)) * CIPHERTEXT_WIDTH+:CIPHERTEXT_WIDTH];
		end
	endgenerate
endmodule
module sky130_sram_1kbyte_1rw1r_32x256_8 (
	vccd1,
	vssd1,
	clk0,
	csb0,
	web0,
	wmask0,
	addr0,
	din0,
	dout0,
	clk1,
	csb1,
	addr1,
	dout1
);
	parameter NUM_WMASKS = 4;
	parameter DATA_WIDTH = 32;
	parameter ADDR_WIDTH = 8;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	parameter DELAY = 3;
	parameter VERBOSE = 1;
	parameter T_HOLD = 1;
	inout vccd1;
	inout vssd1;
	input clk0;
	input csb0;
	input web0;
	input [NUM_WMASKS - 1:0] wmask0;
	input [ADDR_WIDTH - 1:0] addr0;
	input [DATA_WIDTH - 1:0] din0;
	output reg [DATA_WIDTH - 1:0] dout0;
	input clk1;
	input csb1;
	input [ADDR_WIDTH - 1:0] addr1;
	output reg [DATA_WIDTH - 1:0] dout1;
	reg [DATA_WIDTH - 1:0] mem [0:RAM_DEPTH - 1];
	reg csb0_reg;
	reg web0_reg;
	reg [NUM_WMASKS - 1:0] wmask0_reg;
	reg [ADDR_WIDTH - 1:0] addr0_reg;
	reg [DATA_WIDTH - 1:0] din0_reg;
	always @(posedge clk0) begin
		csb0_reg = csb0;
		web0_reg = web0;
		wmask0_reg = wmask0;
		addr0_reg = addr0;
		din0_reg = din0;
		#(T_HOLD) dout0 = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
		if ((!csb0_reg && web0_reg) && VERBOSE)
			$display($time, " Reading %m addr0=%d dout0=%d", addr0_reg, mem[addr0_reg]);
		if ((!csb0_reg && !web0_reg) && VERBOSE)
			$display($time, " Writing %m addr0=%d din0=%d wmask0=%b", addr0_reg, din0_reg, wmask0_reg);
	end
	reg csb1_reg;
	reg [ADDR_WIDTH - 1:0] addr1_reg;
	always @(posedge clk1) begin
		csb1_reg = csb1;
		addr1_reg = addr1;
		if (((!csb0 && !web0) && !csb1) && (addr0 == addr1))
			$display($time, " WARNING: Writing and reading addr0=%d and addr1=%d simultaneously!", addr0, addr1);
		#(T_HOLD) dout1 = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
		if (!csb1_reg && VERBOSE)
			$display($time, " Reading %m addr1=%d dout1=%d", addr1_reg, mem[addr1_reg]);
	end
	always @(negedge clk0) begin : MEM_WRITE0
		if (!csb0_reg && !web0_reg) begin
			if (wmask0_reg[0])
				mem[addr0_reg][7:0] = din0_reg[7:0];
			if (wmask0_reg[1])
				mem[addr0_reg][15:8] = din0_reg[15:8];
			if (wmask0_reg[2])
				mem[addr0_reg][23:16] = din0_reg[23:16];
			if (wmask0_reg[3])
				mem[addr0_reg][31:24] = din0_reg[31:24];
		end
	end
	always @(negedge clk0) begin : MEM_READ0
		if (!csb0_reg && web0_reg)
			dout0 <= #(DELAY) mem[addr0_reg];
	end
	always @(negedge clk1) begin : MEM_READ1
		if (!csb1_reg)
			dout1 <= #(DELAY) mem[addr1_reg];
	end
endmodule