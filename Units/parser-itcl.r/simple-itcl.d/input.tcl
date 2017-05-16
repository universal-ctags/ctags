#
# Derived from class.n man page of itcl
#
package require Itcl

itcl::class Toaster {
    variable crumbs 0
    method toast {nslices} {
        if {$crumbs > 50} {
            error "== FIRE! FIRE! =="
        }
        set crumbs [expr $crumbs+4*$nslices]
    }
    method clean {} {
        set crumbs 0
    }
}

itcl::class SmartToaster {
    inherit Toaster
    method toast {nslices} {
        if {$crumbs > 40} {
            clean
        }
        return [chain $nslices]
    }

    public method doSomethingPublic {} {
    }
    protected method doSomethingProtected {} {
    }
    private method doSomethingPrivate {} {
    }

    proc procNoProtection {} {
    }
    
    public proc procPublic {} {
    }
    protected proc procProtected {} {
    }
    private proc procPrivate {} {
    }

    common commonNoProtection 0
    
    public proc commonPublic "a"
    protected proc commonProtected "b"
    private proc commonPrivate "c"
}

set toaster [SmartToaster #auto]
$toaster toast 2

