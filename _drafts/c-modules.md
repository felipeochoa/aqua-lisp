
## Standard modules built in C

On Windows I ran the following:

```py3
import pkgutil
for loader, name, ispkg in pkgutil.iter_modules():
    if hasattr(loader, "path"):
        if loader.path.lower().endswith("dlls"):
            print("DLL: %s" % name)
    else:
        print("%s: %s" % (loader, name))
```

And came up with the following list of modules:

- **_bz2** The backend to the `bz2` module
- **_ctypes** The backend to the `ctypes` module
- **_ctypes_test** Helps testing a variety of builtin modules, but not
  directly used (I think) in actual Python code
- **_decimal** A speedier version of the `decimal` module
- **_elementtree** A speedier version of the `ElementTree` module
- **_hashlib** Provides a Python bridge to OpenSSL. The mandatory
  algorithms are provided in separate c-extension modules (see below)
- **_lzma** Provides the LZMA compression algorithm, used in the
  `lzma` module
- **_msi** Supports building a Microsoft Installer binary package with
  `distutils`
- **_multiprocessing** Provides some C-level functionality for
  semaphores, sockets, and other multiprocessing-related tasks
- **_overlapped** This module supports the `asyncio` module on Windows
- **_socket** The back end to the `socket` module
- **_sqlite3** The back end to the `sqlite3` module
- **_ssl** The back end to the `ssl` module
- **_testbuffer** This module is used to test the buffer API
- **_testcapi** This module is used to test a variety of builtin
  features, presumably including the C API.
- **_testimportmultiple** This module is used to test issue 16421:
  "loading several modules from the same compiled file fails"
- **_tkinter** This is the TCL interface used by `tkinter`
- **pyexpat** This is the Expat non-validating XML parser, typically
  used through `xml.parsers.expat`
- **select** Provides access to the `select()` and `poll()` (or
  similar) functions. It is used by `asyncore`, `http.server`,
  `subprocess`, `multiprocessing`, and many more
- **unicodedata** This module "provides access to the Unicode
  Character Database (UCD) which defines character properties for all
  Unicode characters"
- **winsound** This module "provides access to the basic sound-playing
  machinery provided by Windows platforms."


Looking through some of these modules, I found some additional ones
that `iter_modules` missed:

- **_sha1** Provides the _SHA-1_ algorithm
- **_md5** Provides the _MD5_ algorithm (defined in RFC 1321)
- **sha256** Provides the _SHA-256_ and _SHA-224_ algorithms
- **sha512** Provides the _SHA-512_ and _SHA-384_ algorithms
