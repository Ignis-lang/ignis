/**
 * Ignis runtime: reference counting (Rc).
 */

#include "../ignis_rt.h"
#include "rt_internal.h"

static size_t rc_payload_align(size_t payload_align) {
  size_t min_align = _Alignof(max_align_t);

  if (payload_align == 0) {
    return min_align;
  }

  // Invalid alignments fall back to max_align_t to keep behavior safe.
  if ((payload_align & (payload_align - 1)) != 0) {
    return min_align;
  }

  return payload_align < min_align ? min_align : payload_align;
}

// The payload starts right after the header, aligned to requested payload alignment.
static size_t rc_header_size(size_t payload_align) {
  return ignis_align_up(sizeof(IgnisRcBox), rc_payload_align(payload_align));
}

IgnisRcBox *ignis_rc_alloc(size_t payload_size, size_t payload_align, IgnisDropFn drop_fn) {
  size_t normalized_align = rc_payload_align(payload_align);
  size_t header = rc_header_size(normalized_align);
  size_t total = header + payload_size;

  if (IGNIS_UNLIKELY(total < header)) {
    return NULL;
  }

  IgnisRcBox *rc = (IgnisRcBox *)ignis_alloc(total);
  if (IGNIS_UNLIKELY(rc == NULL)) {
    return NULL;
  }

  rc->refcount = 1;
  rc->drop_fn = drop_fn;
  rc->payload_size = payload_size;
  rc->payload_align = normalized_align;

  // Zero-initialize the payload
  memset((char *)rc + header, 0, payload_size);

  return rc;
}

void ignis_rc_retain(IgnisRcBox *rc) {
  rc->refcount += 1;
}

void ignis_rc_release(IgnisRcBox *rc) {
  if (IGNIS_UNLIKELY(rc->refcount == 0)) {
    return;
  }

  rc->refcount -= 1;

  if (rc->refcount == 0) {
    if (rc->drop_fn != NULL) {
      void *payload = (char *)rc + rc_header_size(rc->payload_align);
      rc->drop_fn(payload);
    }

    ignis_free(rc);
  }
}

void *ignis_rc_get(IgnisRcBox *rc) {
  return (char *)rc + rc_header_size(rc->payload_align);
}

uint32_t ignis_rc_count(const IgnisRcBox *rc) {
  return rc->refcount;
}
