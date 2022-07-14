# utf8-input-stream

A UTF-8 string input stream over a binary stream for Common Lisp

## Example

```Lisp
(with-open-file (f *p* :direction :input :element-type '(unsigned-byte 8))
    (with-decompressing-stream (zstd-s f)
       (let ((s (make-character-input-stream zstd-s)))
	(print (read-line s))
	(print (read-line s)))))
```
