
keyring::keyring_unlock(keyring = "ecmwfr",
                        password = Sys.getenv("KEYPASS"))

ecmwfr::wf_set_key(user = Sys.getenv("CDSUSER"),
                   key = Sys.getenv("CDSKEY"),
                   service = "cds")
