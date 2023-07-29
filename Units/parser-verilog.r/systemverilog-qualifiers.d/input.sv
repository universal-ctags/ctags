class foo;
extern function void ext_func (x, y);

extern static function void ext_static_func (x, y);

extern protected function void ext_protected_func (x, y);

extern local function void ext_local_func (x, y);

extern pure virtual function void ext_pure_virt_func (x);

pure virtual function void pure_virt_func (x);

// maybe illegal: static and/or virtual can appear only once.
pure virtual static function void pure_virt_static_func (x);

pure virtual protected function void pure_virt_protected_func (x);

pure virtual local function void pure_virt_local_func (x);


extern task ext_task (x, y);

extern static task ext_static_task (x, y);

extern protected task ext_protected_task (x, y);

extern local task ext_local_task (x, y);

extern pure virtual task ext_pure_virt_task (x);

pure virtual task pure_virt_task (x);

// maybe illegal: static and/or virtual can appear only once.
pure virtual static task pure_virt_static_task (x);

pure virtual protected task pure_virt_protected_task (x);

pure virtual local task pure_virt_local_task (x);

function automatic auto_function (x);
endfunction : auto_function

function static auto_static (x);
endfunction : auto_static

endclass : foo
