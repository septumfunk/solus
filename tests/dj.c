#include "ctr/vm.h"
#include "sf/containers/buffer.h"
#include "sf/fs.h"
#include "sf/str.h"
#include <ctr/bytecode.h>
#include <stdio.h>

sf_buffer load_file(sf_str name) {
    assert(sf_file_exists(name));
    sf_fsb_ex fsb = sf_file_buffer(name);
    if (!fsb.is_ok) {
        switch (fsb.value.err) {
            case SF_FILE_NOT_FOUND: fprintf(stderr, "%s not found\n", name.c_str);
            case SF_OPEN_FAILURE: fprintf(stderr, "%s failed to open\n", name.c_str);
            case SF_READ_FAILURE: fprintf(stderr, "%s failed to read\n", name.c_str);
        }
        return (sf_buffer){.size = SIZE_MAX};
    }
    fsb.value.ok.flags = SF_BUFFER_GROW;
    sf_buffer_autoins(&fsb.value.ok, ""); // [\0]
    return fsb.value.ok;
}

int main(void) {
    sf_buffer dj_s = load_file(sf_lit("ctr.tests/dj.csm"));
    sf_buffer join_s = load_file(sf_lit("ctr.tests/join.csm"));
    if (dj_s.size == SIZE_MAX || join_s.size == SIZE_MAX)
        return -1;
    ctr_state *s = ctr_state_new();

    ctr_asm_ex ex = ctr_assemble(sf_ref((char *)dj_s.ptr));
    if (!ex.is_ok) {
        fprintf(stderr, "dj.csm failed to assemble\n");
        sf_str_free(ex.value.err.string);
        return -1;
    }
    ctr_proto dj = ex.value.ok;

    ex = ctr_assemble(sf_ref((char *)join_s.ptr));
    if (!ex.is_ok) {
        fprintf(stderr, "join.csm failed to assemble\n");
        sf_str_free(ex.value.err.string);
        return -1;
    }
    ctr_proto join = ex.value.ok;
    ctr_val join_v = ctr_dnew(CTR_DFUN);
    *(ctr_dfun *)join_v.val.dyn = &join;

    ctr_call_ex cex = ctr_call(s, &dj, (ctr_val[]){join_v});
    if (!ex.is_ok) {
        fprintf(stderr, "%s\n", cex.value.err.string.c_str);
        sf_str_free(cex.value.err.string);
        return -1;
    }
    ctr_val ret = cex.value.ok;
    sf_str st = ctr_tostring(ret);
    printf("[RET]: %s\n", st.c_str);
    sf_str_free(st);

    ctr_state_free(s);
}
