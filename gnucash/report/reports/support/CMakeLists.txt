set(stylesheets_DATA
  taxinvoice.css
  receipt.css
  balsheet-eg.css
)

install(FILES ${stylesheets_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/stylesheets)

set(templates_DATA
  taxinvoice.eguile.scm
  receipt.eguile.scm
  balsheet-eg.eguile.scm
)

install(FILES ${templates_DATA}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/templates)

set(support_SCRIPTS
  benchmark-fetch.py
  price-fetch.py
)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/benchmark-fetch.py
               ${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}/gnucash/reports/support/benchmark-fetch.py
               COPYONLY)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/price-fetch.py
               ${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}/gnucash/reports/support/price-fetch.py
               COPYONLY)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/benchmark-python.txt.in
               ${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}/gnucash/reports/support/benchmark-python.txt
               @ONLY)

install(FILES ${support_SCRIPTS}
              ${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}/gnucash/reports/support/benchmark-python.txt
        DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/gnucash/reports/support)

set(support_EXTRA_DIST
  README
  benchmark-python.txt.in
)

set_local_dist(reports_support_DIST_local CMakeLists.txt
  ${stylesheets_DATA} ${templates_DATA} ${support_SCRIPTS} ${support_EXTRA_DIST})
set(reports_support_DIST ${reports_support_DIST_local} PARENT_SCOPE)
