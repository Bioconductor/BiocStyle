.defunct <- function (new,
                      package = NULL,
                      msg,
                      old = as.character(sys.call(sys.parent()))[1L]) {
                        if (missing(msg)) {
                          msg <- gettextf("'%s' is deprecated.\n", old)
                          if (!missing(new)) 
                            msg <- c(msg, gettextf("Use '%s' instead.\n", new))
                          c(msg, if (!is.null(package)) gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").", 
                                                                 package) else gettext("See help(\"Deprecated\")"))
                        }
                        .Defunct(new, package, msg)
                      }
