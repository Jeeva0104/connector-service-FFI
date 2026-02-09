#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

extern "C" {

#if defined(FFI_C_FFI)
/// Authorize a payment - C FFI compatible
///
/// # Safety
/// The caller must ensure that `request_json` is a valid null-terminated C string.
/// The returned pointer must be freed by calling `free_string`.
char *authorize_c(const char *request_json);
#endif

#if defined(FFI_C_FFI)
/// Free a string allocated by this library
///
/// # Safety
/// The pointer must have been returned by `authorize_c`.
void free_string(char *ptr);
#endif

}  // extern "C"
