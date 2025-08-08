module Control_Unit (
    input wire clk,
    input wire rst_n,
    input wire [6:0] instruction_opcode,
    output reg pc_write,
    output reg ir_write,
    output reg pc_source,
    output reg reg_write,
    output reg memory_read,
    output reg is_immediate,
    output reg memory_write,
    output reg pc_write_cond,
    output reg lorD,
    output reg memory_to_reg,
    output reg [1:0] aluop,
    output reg [1:0] alu_src_a,
    output reg [1:0] alu_src_b
);

    // Definição dos estados da máquina de controle
    localparam FETCH      = 4'b0000;
    localparam DECODE     = 4'b0001;
    localparam MEMADR     = 4'b0010;
    localparam MEMREAD    = 4'b0011;
    localparam MEMWB      = 4'b0100;
    localparam MEMWRITE   = 4'b0101;
    localparam EXECUTER   = 4'b0110;
    localparam ALUWB      = 4'b0111;
    localparam EXECUTEI   = 4'b1000;
    localparam JAL        = 4'b1001;
    localparam BRANCH     = 4'b1010;
    localparam JALR       = 4'b1011;
    localparam AUIPC      = 4'b1100;
    localparam LUI        = 4'b1101;
    localparam JALR_PC    = 4'b1110;

    // Definição dos opcodes RISC-V utilizados
    localparam LW      = 7'b0000011;
    localparam SW      = 7'b0100011;
    localparam RTYPE   = 7'b0110011;
    localparam ITYPE   = 7'b0010011;
    localparam JALI    = 7'b1101111;
    localparam BRANCHI = 7'b1100011;
    localparam JALRI   = 7'b1100111;
    localparam AUIPCI  = 7'b0010111;
    localparam LUII    = 7'b0110111;

    reg [3:0] state, next_state;

    // Atualiza o estado a cada borda de clock ou reset
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n)
            state <= FETCH;
        else
            state <= next_state;
    end

    // Define a lógica de transição entre estados
    always @(*) begin
        case (state)
            FETCH:      next_state = DECODE;
            DECODE: begin
                case (instruction_opcode)
                    LW:      next_state = MEMADR;
                    SW:      next_state = MEMADR;
                    RTYPE:   next_state = EXECUTER;
                    ITYPE:   next_state = EXECUTEI;
                    JALI:    next_state = JAL;
                    BRANCHI: next_state = BRANCH;
                    JALRI:   next_state = JALR;
                    AUIPCI:  next_state = AUIPC;
                    LUII:    next_state = LUI;
                    default: next_state = FETCH;
                endcase
            end
            MEMADR:   next_state = (instruction_opcode == LW) ? MEMREAD : MEMWRITE;
            MEMREAD:  next_state = MEMWB;
            MEMWB:    next_state = FETCH;
            MEMWRITE: next_state = FETCH;
            EXECUTER: next_state = ALUWB;
            EXECUTEI: next_state = ALUWB;
            ALUWB:    next_state = FETCH;
            JAL:      next_state = ALUWB;
            BRANCH:   next_state = FETCH;
            JALR:     next_state = JALR_PC;
            JALR_PC:  next_state = ALUWB;
            AUIPC:    next_state = ALUWB;
            LUI:      next_state = ALUWB;
            default:  next_state = FETCH;
        endcase
    end

    // Define os sinais de controle gerados em cada estado
    always @(*) begin
        // Valores padrão (todos desligados)
        {pc_write, ir_write, pc_source, reg_write, memory_read, is_immediate,
         memory_write, pc_write_cond, lorD, memory_to_reg, aluop,
         alu_src_a, alu_src_b} = 0;

        case (state)
            FETCH: begin
                memory_read   = 1;
                alu_src_a     = 2'b00;
                alu_src_b     = 2'b01;
                aluop         = 2'b00;
                ir_write      = 1;
                pc_write      = 1;
                pc_source     = 0;
            end
            DECODE: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end
            MEMADR: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end
            MEMREAD: begin
                memory_read   = 1;
                lorD          = 1;
            end
            MEMWB: begin
                reg_write     = 1;
                memory_to_reg = 2'b01;
            end
            MEMWRITE: begin
                memory_write  = 1;
                lorD          = 1;
            end
            EXECUTER: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b00;
                aluop         = 2'b10;
            end
            EXECUTEI: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b10;
                is_immediate  = 1;
            end
            ALUWB: begin
                reg_write     = 1;
                memory_to_reg = 2'b00;
            end
            BRANCH: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b00;
                aluop         = 2'b01;
                pc_write_cond = 1;
                pc_source     = 1;
            end
            JAL: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b01;
                pc_write      = 1;
                pc_source     = 1;
                aluop         = 2'b00;
            end
            JALR: begin
                alu_src_a     = 2'b01;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end
            JALR_PC: begin
                pc_write      = 1;
                pc_source     = 1;
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b01;
                is_immediate  = 1;
            end
            AUIPC: begin
                alu_src_a     = 2'b10;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end
            LUI: begin
                alu_src_a     = 2'b11;
                alu_src_b     = 2'b10;
                aluop         = 2'b00;
            end
        endcase
    end

endmodule
