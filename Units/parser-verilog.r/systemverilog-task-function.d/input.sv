// LRM 13.5.2 Pass by Reference
module task_func;

  function automatic int crc( ref byte packet [1000:1] );
    for( int j= 1; j <= 1000; j++ ) begin
      crc ^= packet[j];
    end
  endfunction

  task automatic show ( const ref byte data [] );
    for ( int j = 0; j < data.size ; j++ )
      $display( data[j] ); // data can be read but not written
  endtask

  task automatic attr ( (* my_attr *) const ref foo, enum { s0, s1 } sel_e );
  endtask

endmodule

// disabled K_PROTOTYPE: don't use "--extras=+q" for coverage
class C;
  extern function ext_func ();
  pure virtual task ext_pure_virt_task (x);
  typedef class   fwd_type_class;
endclass
