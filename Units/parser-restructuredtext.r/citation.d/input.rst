=================
KVM VCPU Requests
=================

This text is partially taken from documentation/virt/kvm/vcpu-requests.rst of
Linux kernel.

...

VCPU Request Internals
======================

VCPU requests are simply bit indices of the ``vcpu->requests`` bitmap.
This means general bitops, like those documented in [atomic-ops]_ could
also be used, e.g. ::

  clear_bit(KVM_REQ_UNHALT & KVM_REQUEST_MASK, &vcpu->requests);

However, VCPU request users should refrain from doing so, as it would
break the abstraction.  The first 8 bits are reserved for architecture
independent requests, all additional bits are available for architecture
dependent requests.

...

References
==========

.. [atomic-ops] Documentation/core-api/atomic_ops.rst
.. [memory-barriers] Documentation/memory-barriers.txt
.. [lwn-mb] https://lwn.net/Articles/573436/
