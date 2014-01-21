test.anonymous <- function() {
  central <- XNAT('https://central.xnat.org')
  close(central)
}

test.login <- function() {
  central <- XNAT('https://central.xnat.org', 'nosetests', 'nosetests')
  close(central)
}

test.bad.password <- function() {
  checkException(XNAT('https://central.xnat.org', 'nosetests', 'bogus'))
}

# eof
