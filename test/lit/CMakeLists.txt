configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

find_program(LLVM_LIT lit)
set(LLVM_EXTERNAL_LIT ${LLVM_LIT} CACHE INTERNAL "")

add_lit_testsuite(check-bara "Running bara regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS bara
) 
