//
//  LRM: 19. Functional coverage
//
// covergroup is decleared in class, package, or checker

// 19.3 Defining the coverage model: covergroup
checker C0 (logic clk);
    enum { red, green, blue } color;
    bit [3:0] pixel_adr, pixel_offset, pixel_hue;

    covergroup g2 @(posedge clk);
        Hue: coverpoint pixel_hue;
        Offset: coverpoint pixel_offset;

        AxC: cross color, pixel_adr;    // cross 2 variables (implicitly declared
                                        // coverpoints)

        all: cross color, Hue, Offset;  // cross 1 variable and 2 coverpoints
    endgroup
endchecker

// 19.4 Using covergroup in classes
class xyz;
    bit [3:0] m_x;
    int m_y;
    bit m_z;

    covergroup cov1 @m_z;   // embedded covergroup
        coverpoint m_x;
        coverpoint m_y;
    endgroup

    function new(); cov1 = new; endfunction
endclass

// 19.5.1 Specifying bins for values
checker C1 (logic clk);
    bit [9:0] v_a;

    covergroup cg @(posedge clk);
        coverpoint v_a
        {
            bins a = { [0:63],65 };
            bins b[] = { [127:150],[148:191] }; // note overlapping values
            bins c[] = { 200,201,202 };
            bins d = { [1000:$] };
            bins others[] = default;
        }
    endgroup
endchecker

// 19.6 Defining cross coverage
checker C2 (logic clk);
    bit [3:0] a, b, c;
    covergroup cov2 @(posedge clk);
        BC: coverpoint b+c;
        aXb : cross a, BC;
    endgroup
endchecker

// 19.8.1 Overriding the built-in sample method
checker C3 (logic clk);
    covergroup p_cg with function sample(bit a, int x);
        coverpoint x;
        cross x, a;
    endgroup : p_cg
endchecker

// original
checker C4 (logic clk);
    covergroup cg @@ ( begin task_end );
    endgroup

    reg var_to_check_context;
endchecker
