;;;
;;; Given the parser output (an AST using internal reprensentation nodes as
;;; defined in src/structs.lisp), prepare the AST to be output as a PL/pgSQL
;;; function.

(in-package #:plconvert)

#|
https://github.com/keithf4/oracle_fdw_snapshot/blob/master/ofdw.create_oracle_fdw_table.sql

CASE WHEN data_type = 'VARCHAR' THEN 'text' 
        WHEN data_type = 'VARCHAR2' THEN 'text'
        WHEN data_type = 'NVARCHAR2' THEN 'text'  
        WHEN data_type = 'CHAR' THEN 'char'||'('||data_length||')'
        WHEN data_type = 'NCHAR' THEN 'char'||'('||data_length||')'
        WHEN data_type = 'CLOB' THEN 'text'
        WHEN data_type = 'CFILE' THEN 'text'
        WHEN data_type = 'RAW' THEN 'bytea'
        WHEN data_type = 'BLOB' THEN 'bytea'
        WHEN data_type = 'BFILE' THEN 'bytea'
        WHEN data_type = 'NUMBER' THEN 'numeric'
        WHEN data_type = 'FLOAT' THEN 'float8'
        WHEN data_type = 'BINARY_FLOAT' THEN 'float8'
        WHEN data_type = 'DATE' THEN 'timestamp'
        WHEN data_type = 'TIMESTAMP' THEN 'timestamp'
        WHEN data_type = 'TIMESTAMP WITH TIME ZONE' THEN 'timestamptz'
        WHEN data_type = 'INTERVAL YEAR TO MONTH' THEN 'interval'
        WHEN data_type = 'INTERVAL DAY TO SECOND' THEN 'interval'
        WHEN data_type = 'LONG' THEN 'text'
        WHEN data_type = 'LONG RAW' THEN 'bytea'
        ELSE 'WTF->' || data_type END
|#

(defvar *oracle-data-type-mapping*
  (alexandria:alist-hash-table '(("VARCHAR"                  . "text")
                                 ("VARCHAR2"                 . "text")
                                 ("NVARCHAR2"                . "text")
                                 ("CHAR"                     . "text")
                                 ("NCHAR"                    . "text")
                                 ("CLOB"                     . "text")
                                 ("CFILE"                    . "text")
                                 ("RAW"                      . "bytea")
                                 ("BLOB"                     . "bytea")
                                 ("BFILE"                    . "bytea")
                                 ("NUMBER"                   . "numeric")
                                 ("FLOAT"                    . "float")
                                 ("BINARY_FLOAT"             . "float8")
                                 ("DATE"                     . "timestamptz")
                                 ("TIMESTAMP"                . "timestamp")
                                 ("TIMESTAMP WITH TIME ZONE" . "timestamptz")
                                 ("INTERVAL YEAR TO MONTH"   . "interval")
                                 ("INTERVAL DAY TO SECOND"   . "interval")
                                 ("LONG"                     . "text")
                                 ("LONG RAW"                 . "bytea"))
                               :test 'equal))

(defun convert-data-type (data-type)
  "Map given Oracle data type CNAME into a PostgreSQL suitable data type."
  (when (and (null (cname-schema (data-type-cname data-type)))
             (null (cname-relname (data-type-cname data-type))))

    (let ((data-type-name (cname-attribute (data-type-cname data-type))))
      (setf (cname-attribute (data-type-cname data-type))
            (gethash data-type-name *oracle-data-type-mapping* data-type-name)))))

