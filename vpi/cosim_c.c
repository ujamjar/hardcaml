// Icarus verilog VPI interface
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "vpi_user.h"

#include "caml/alloc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/callback.h"

/* constants */

struct constant {
  char *name;
  uint32_t value;
};

#define C(n) { .name = #n, .value = n }

static struct constant constants[] = {
  C(vpiSysTask                ),
  C(vpiSysFunc                ),

  C(vpiScaledRealTime         ),
  C(vpiSimTime                ),
  C(vpiSuppressTime           ),

  C(vpiBinStrVal              ),
  C(vpiOctStrVal              ),
  C(vpiDecStrVal              ),
  C(vpiHexStrVal              ),
  C(vpiScalarVal              ),
  C(vpiIntVal                 ),
  C(vpiRealVal                ),
  C(vpiStringVal              ),
  C(vpiVectorVal              ),
  C(vpiStrengthVal            ),
  C(vpiTimeVal                ),
  C(vpiObjTypeVal             ),
  C(vpiSuppressVal            ),

  C(vpi0                      ),
  C(vpi1                      ),
  C(vpiZ                      ),
  C(vpiX                      ),
  C(vpiH                      ),
  C(vpiL                      ),
  C(vpiDontCare               ),

  C(vpiSupplyDrive            ),
  C(vpiStrongDrive            ),
  C(vpiPullDrive              ),
  C(vpiLargeCharge            ),
  C(vpiWeakDrive              ),
  C(vpiMediumCharge           ),
  C(vpiSmallCharge            ),
  C(vpiHiZ                    ),
                              
  C(vpiConstant               ),
  C(vpiFunction               ),
  C(vpiIntegerVar             ),
  C(vpiIterator               ),
  C(vpiMemory                 ),
  C(vpiMemoryWord             ),
  C(vpiModPath                ),
  C(vpiModule                 ),
  C(vpiNamedBegin             ),
  C(vpiNamedEvent             ),
  C(vpiNamedFork              ),
  C(vpiNet                    ),
  C(vpiParameter              ),
  C(vpiPartSelect             ),
  C(vpiPathTerm               ),
  C(vpiRealVar                ),
  C(vpiReg                    ),
  C(vpiSysFuncCall            ),
  C(vpiSysTaskCall            ),
  C(vpiTask                   ),
  C(vpiTimeVar                ),
  C(vpiNetArray               ),
  C(vpiIndex                  ),
  C(vpiLeftRange              ),
  C(vpiParent                 ),
  C(vpiRightRange             ),
  C(vpiScope                  ),
  C(vpiSysTfCall              ),
  C(vpiArgument               ),
  C(vpiInternalScope          ),
  C(vpiModPathIn              ),
  C(vpiModPathOut             ),
  C(vpiVariables              ),
  C(vpiExpr                   ),
                              
  C(vpiCallback               ),

  C(vpiUndefined              ),
  C(vpiType                   ),
  C(vpiName                   ),
  C(vpiFullName               ),
  C(vpiSize                   ),
  C(vpiFile                   ),
  C(vpiLineNo                 ),
  C(vpiTopModule              ),
  C(vpiCellInstance           ),
  C(vpiDefName                ),
  C(vpiTimeUnit               ),
  C(vpiTimePrecision          ),
  C(vpiDefFile                ),
  C(vpiDefLineNo              ),
  C(vpiNetType                ),
  C(vpiWire                   ),
  C(vpiWand                   ),
  C(vpiWor                    ),
  C(vpiTri                    ),
  C(vpiTri0                   ),
  C(vpiTri1                   ),
  C(vpiTriReg                 ),
  C(vpiTriAnd                 ),
  C(vpiTriOr                  ),
  C(vpiSupply1                ),
  C(vpiSupply0                ),
  C(vpiArray                  ),
  C(vpiEdge                   ),
  C(vpiNoEdge                 ),
  C(vpiEdge01                 ),
  C(vpiEdge10                 ),
  C(vpiEdge0x                 ),
  C(vpiEdgex1                 ),
  C(vpiEdge1x                 ),
  C(vpiEdgex0                 ),
  C(vpiPosedge                ),
  C(vpiNegedge                ),
  C(vpiAnyEdge                ),
  C(vpiConstType              ),
  C(vpiDecConst               ),
  C(vpiRealConst              ),
  C(vpiBinaryConst            ),
  C(vpiOctConst               ),
  C(vpiHexConst               ),
  C(vpiStringConst            ),
  C(vpiFuncType               ),
  C(vpiIntFunc                ),
  C(vpiRealFunc               ),
  C(vpiTimeFunc               ),
  C(vpiSizedFunc              ),
  C(vpiSizedSignedFunc        ),
  C(vpiSysFuncType            ),
  C(vpiSysFuncInt             ),
  C(vpiSysFuncReal            ),
  C(vpiSysFuncTime            ),
  C(vpiSysFuncSized           ),
  C(vpiAutomatic              ),
  C(vpiConstantSelect         ),
  C(vpiSigned                 ),
  C(_vpiNexusId               ),
                              
  C(vpiNoDelay                ),
  C(vpiInertialDelay          ),
  C(vpiTransportDelay         ),
  C(vpiPureTransportDelay     ),

  C(vpiForceFlag              ),
  C(vpiReleaseFlag            ),
  C(vpiReturnEvent            ),
                              
  C(cbValueChange             ),
  C(cbStmt                    ),
  C(cbForce                   ),
  C(cbRelease                 ),
  C(cbAtStartOfSimTime        ),
  C(cbReadWriteSynch          ),
  C(cbReadOnlySynch           ),
  C(cbNextSimTime             ),
  C(cbAfterDelay              ),
  C(cbEndOfCompile            ),
  C(cbStartOfSimulation       ),
  C(cbEndOfSimulation         ),
  C(cbError                   ),
  C(cbTchkViolation           ),
  C(cbStartOfSave             ),
  C(cbEndOfSave               ),
  C(cbStartOfRestart          ),
  C(cbEndOfRestart            ),
  C(cbStartOfReset            ),
  C(cbEndOfReset              ),
  C(cbEnterInteractive        ),
  C(cbExitInteractive         ),
  C(cbInteractiveScopeChange  ),
  C(cbUnresolvedSystf         ),


  C(vpiStop                   ),
  C(vpiFinish                 ),
  C(vpiReset                  ),
  C(vpiSetInteractiveScope    ),
  C(__ivl_legacy_vpiStop      ),
  C(__ivl_legacy_vpiFinish    ),

  C(vpiCompile                ),
  C(vpiPLI                    ),
  C(vpiRun                    ),
                              
  C(vpiNotice                 ),
  C(vpiWarning                ),
  C(vpiError                  ),
  C(vpiSystem                 ),
  C(vpiInternal               ),
                              
  C(_vpiFromThr               ),
  C(_vpiNoThr                 ),
  C(_vpiString                ),
  C(_vpiVThr                  ),
  C(_vpiWord                  ),
  C(_vpi_at_PV                ),
  C(_vpi_at_A                 ),
  C(_vpi_at_APV               ),
};

extern uint32_t vpi_get_constant(char *, int);
uint32_t vpi_get_constant(char *name, int value) {
  int i=0;
  while (i<(sizeof(constants)/sizeof(struct constant))) {
    if (0 == strcmp(constants[i].name, name)) 
      return (value ? constants[i].value : 1);
    i++;
  }
  return 0;
}

/* shim into ocaml */

static void init_vpi_caml(void) {
  CAMLparam0();
  value * func = caml_named_value("init_vpi");
  if (func == NULL) {
    fprintf(stderr, "failed to call init_vpi (ocaml)\n");
    exit(1);
  } else {
    caml_callback(*func, Val_unit);
  }
  CAMLreturn0;
}

static char *dummy_argv[] = { "vpi_exe" };

static void init_vpi_c(void) {
  caml_startup(dummy_argv);
  init_vpi_caml();
}

void (*vlog_startup_routines[])() = {
      init_vpi_c,
      0
};

#ifdef MAIN
int main(int argc, char *argv[]) {
  init_vpi_c();
  return 0;
}
#endif
