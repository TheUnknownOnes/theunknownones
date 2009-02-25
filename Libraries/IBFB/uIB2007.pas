//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uIB2007;

interface

uses
  uIBFBGLobals,
  DateUtils;

const
  gds32dll = 'gds32.dll';
  ib_utildll = 'ib_util.dll';
  
  ISC_TRUE	= 1;
  ISC_FALSE	= 0;

  ISC__TRUE	= ISC_TRUE;
  ISC__FALSE	= ISC_FALSE;

type
  ISC_LONG	= long; PISC_LONG = ^ISC_LONG;
  ISC_ULONG	= unsigned_long; PISC_ULONG = ^ISC_ULONG;

  ISC_BOOLEAN	= signed_short; PISC_BOOLEAN = ^ISC_BOOLEAN;

  ISC_USHORT	= unsigned_short; PISC_USHORT = ^ISC_USHORT;
  ISC_STATUS	= long; PISC_STATUS = ^ISC_STATUS;

  { START CONVERT TAG }
const
  DSQL_close     = 1;
  DSQL_drop      = 2;
  DSQL_cancel    = 4;

  METADATALENGTH	= 68;

  { END CONVERT TAG }

  {****************************************************************}
  { Define type, export and other stuff based on c/c++ and Windows }
  {****************************************************************}


  {*****************************************************************}
  { 64 bit Integers                                                 }
  {*****************************************************************}

type
  ISC_INT64 = __int64; PISC_INT64 = ^ISC_INT64;
  ISC_UINT64 = unsigned___int64; PISC_UINT64 = ^ISC_UINT64;

  {*****************************************************************}
  { Time & Date Support                                             }
  {*****************************************************************}

type
  ISC_DATE = long; PISC_DATE = ^ISC_DATE;
  ISC_TIME = unsigned_long; PISC_TIME = ^ISC_TIME;
  ISC_TIMESTAMP = packed record
    timestamp_date : ISC_DATE;
    timestamp_time : ISC_TIME;
  end;
  PISC_TIMESTAMP = ^ISC_TIMESTAMP;

const
  _ISC_TIMESTAMP_			= 1;

  ISC_TIME_SECONDS_PRECISION          = 10000;
  ISC_TIME_SECONDS_PRECISION_SCALE    = -4;

  {*****************************************************************}
  { Blob id structure                                               }
  {*****************************************************************}

type
  GDS_QUAD = packed record
    gds_quad_high : ISC_LONG;
    gds_quad_low : ISC_ULONG;
  end;
  PGDS_QUAD = ^GDS_QUAD;
  GDS__QUAD = GDS_QUAD; PGDS__QUAD = ^GDS__QUAD;

  ISC_QUAD = GDS_QUAD; PISC_QUAD = ^ISC_QUAD;

  ISC_ARRAY_BOUND = packed record
    array_bound_lower : short;
    array_bound_upper : short;
  end;
  PISC_ARRAY_BOUND = ^ISC_ARRAY_BOUND;

  ISC_ARRAY_DESC_V2 = packed record
    array_desc_version : short;
    array_desc_dtype : unsigned_char;
    array_desc_subtype : unsigned_char;
    array_desc_scale : AnsiChar;
    array_desc_length : unsigned_short;
    array_desc_field_name : array[0 .. METADATALENGTH - 1] of AnsiChar;
    array_desc_relation_name : array[0 .. METADATALENGTH - 1] of AnsiChar;
    array_desc_dimensions : short;
    array_desc_flags : short;
    array_desc_bounds : array[0 .. 15] of ISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC_V2 = ^ISC_ARRAY_DESC_V2;

const
  ARR_DESC_VERSION2		= 2;
  ARR_DESC_CURRENT_VERSION 	= ARR_DESC_VERSION2;

type
  ISC_BLOB_DESC_V2 = packed record
    blob_desc_version : short;
    blob_desc_subtype : short;
    blob_desc_charset : short;
    blob_desc_segment_size : short;
    blob_desc_field_name : array[0 .. METADATALENGTH - 1] of unsigned_char;
    blob_desc_relation_name : array[0 .. METADATALENGTH - 1] of unsigned_char;
  end;
  PISC_BLOB_DESC_V2 = ^ISC_BLOB_DESC_V2;

const
  BLB_DESC_VERSION2		= 2;
  BLB_DESC_CURRENT_VERSION 	= BLB_DESC_VERSION2;

  {*************************}
  { Blob control structure  }
  {*************************}

type
  ISC_BLOB_CTL_SOURCE_FUNCTION = function() : ISC_STATUS;

  PISC_BLOB_CTL = ^ISC_BLOB_CTL;
  ISC_BLOB_CTL = packed record
    ctl_source : ISC_BLOB_CTL_SOURCE_FUNCTION;	{ Source filter }
    ctl_source_handle : PISC_BLOB_CTL; { Argument to pass to source }
            { filter }
    ctl_to_sub_type : short;  	{ Target type }
    ctl_from_sub_type : short;	{ Source type }
    ctl_buffer_length : unsigned_short;	{ Length of buffer }
    ctl_segment_length: unsigned_short;  	{ Length of current segment }
    ctl_bpb_length : unsigned_short;	{ Length of blob parameter }
                { block }
    ctl_bpb : PAnsiChar;		{ Address of blob parameter }
            { block }
    ctl_buffer : Punsigned_char;		{ Address of segment buffer }
    ctl_max_segment : ISC_LONG;	{ Length of longest segment }
    ctl_number_segments : ISC_LONG; 	{ Total number of segments }
    ctl_total_length : ISC_LONG;  	{ Total length of blob }
    ctl_status : PISC_STATUS;		{ Address of status vector }
    ctl_data : array [0 .. 7] of long;	  	{ Application specific data }
  end;


  {*************************}
  { Blob stream definitions } 
  {*************************}

type
  BSTREAM = packed record
    bstr_blob : Pointer;  	{ Blob handle }
    bstr_buffer : PAnsiChar;	{ Address of buffer }
    bstr_ptr : PAnsiChar;	{ Next character }
    bstr_length : short;		{ Length of buffer }
    bstr_cnt : short;		{ Characters in buffer }
    bstr_mode : Char;  		{ (mode) ? OUTPUT : INPUT }
  end;
  PBSTREAM = ^BSTREAM;

  function getb(var p: BSTREAM): AnsiChar;
  function putb(x: AnsiChar; var p: BSTREAM): Int;
  function putbx(x: AnsiChar; var p: BSTREAM): Int;
  

  {**********************************************************************
   * Older and obsolete XSQLVAR, ISC_BLOB_DESC, ISC_ARRAY_DESC strucutres.
   * NOTE:These structure will no longer be available in future releases.
   * This is kept only for backward  compatability. 
   * Please refrain from  using these old structures. 
   * It is strongly  recomended  to use the newer SQLDA version
   * and related XSQLVAR, ISC_BLOB_DESC, ISC_ARRAY_DESC structures.
   **********************************************************************}
type
  XSQLVAR_V1 = packed record
      sqltype : short;		{ datatype of field }
      sqlscale : short;		{ scale factor }
      sqlsubtype : short;		{ datatype subtype }
      sqllen : short;			{ length of data area }
      sqldata : PAnsiChar;		{ address of data }
      sqlind : PShort;		{ address of indicator variable }
      sqlname_length : short;		{ length of sqlname field }
      sqlname : array[0 .. 31] of AnsiChar;		{ name of field, name length + space }
            { for NULL }
      relname_length : short;		{ length of relation name }
      relname : array[0 .. 31] of AnsiChar;		{ field's relation name + space for }
            { NULL }
      ownname_length : short;		{ length of owner name }
      ownname : array[0 .. 31] of AnsiChar;		{ relation's owner name + space for }
            { NULL }
      aliasname_length : Short; 	{ length of alias name }
      aliasname : array[0 .. 31] of AnsiChar;		{ relation's alias name + space for }
            { NULL }
  end;
  PXSQLVAR_V1 = ^XSQLVAR_V1;

const
  SQLDA_VERSION1		        = 1;

type
  ISC_ARRAY_DESC = packed record
    array_desc_dtype : unsigned_char;
    array_desc_scale : AnsiChar;
    array_desc_length : unsigned_short;
    array_desc_field_name : array[0 .. 31] of AnsiChar;
    array_desc_relation_name : array[0 .. 31] of AnsiChar;
    array_desc_dimensions : short;
    array_desc_flags : short;
    array_desc_bounds : array[0 .. 15] of ISC_ARRAY_BOUND;
  end; 
  PISC_ARRAY_DESC = ^ISC_ARRAY_DESC;

  ISC_BLOB_DESC = packed record
    blob_desc_subtype : short;
    blob_desc_charset : short;
    blob_desc_segment_size : short;
    blob_desc_field_name : array[0 .. 31] of unsigned_char;
    blob_desc_relation_name : array[0 .. 31] of unsigned_char;
  end;
  PISC_BLOB_DESC = ^ISC_BLOB_DESC;

  {*********************************************}

  {*************************}
  { Dynamic SQL definitions }
  {*************************}
 
  {****************************}
  { Declare the extended SQLDA }
  {****************************}

type
  XSQLVAR = packed record
    sqltype : short;		{ datatype of field }
    sqlscale : short;		{ scale factor }
    sqlprecision : short;		{ precision : Reserved for future }
    sqlsubtype : short;		{ datatype subtype }
    sqllen : short;			{ length of data area }
    sqldata : PAnsiChar;		{ address of data }
    sqlind : Pshort;		{ address of indicator variable }
    sqlname_length : short;		{ length of sqlname field }
    sqlname : array[0 .. METADATALENGTH - 1] of AnsiChar;		{ name of field, name length + space }
          { for NULL }
    relname_length : short;		{ length of relation name }
    relname : array[0 .. METADATALENGTH - 1] of AnsiChar;		{ field's relation name + space for }
          { NULL }
    ownname_length : short;		{ length of owner name }
    ownname : array[0 .. METADATALENGTH - 1] of AnsiChar;		{ relation's owner name + space for }
          { NULL }
    aliasname_length : short; 	{ length of alias name }
    aliasname : array[0 .. METADATALENGTH - 1] of AnsiChar;		{ relation's alias name + space for }
          { NULL }
  end;
  PXSQLVAR = ^XSQLVAR;

  XSQLDA = packed record
    version : short;		{ version of this XSQLDA }
    sqldaid : array[0 .. 7] of AnsiChar;		{ XSQLDA name field }
    sqldabc : ISC_LONG;		{ length in bytes of SQLDA }
    sqln : short;			{ number of fields allocated }
    sqld : short;			{ actual number of fields }
    sqlvar : array[0 .. 0] of XSQLVAR;		{ first field address }
  end;
  PXSQLDA = ^XSQLDA;

const
  SQLDA_VERSION2		        = 2;
  SQLDA_CURRENT_VERSION           = SQLDA_VERSION2;

  function XSQLDA_LENGTH(n : Long) : Long;
  function XSQLVAR_LENGTH(num_rows, num_vars : Long) : Long;
  //#define XSQLDA_LENGTH(n)		(sizeof (XSQLDA) + (n) * sizeof (XSQLVAR))
  //#define XSQLVAR_LENGTH(num_rows, num_vars) (sizeof(XSQLVAR) * num_rows * num_vars)

  { START CONVERT TAG }
const
  SQL_DIALECT_V5			= 1; { meaning is same as DIALECT_xsqlda }
  SQL_DIALECT_V6_TRANSITION	= 2; { flagging anything that is delimited
                                              by double quotes as an error and
                                              flagging keyword DATE as an error }
  SQL_DIALECT_V6			= 3; { supports SQL delimited identifier,
                                              SQLDATE/DATE, TIME, TIMESTAMP,
                                              CURRENT_DATE, CURRENT_TIME,
                                              CURRENT_TIMESTAMP, and 64-bit exact
                                              numeric type }
  SQL_DIALECT_CURRENT		= SQL_DIALECT_V6; { latest IB DIALECT }
  { END CONVERT TAG }

  {******************************}
  { InterBase Handle Definitions }
  {******************************}

type
  isc_att_handle = Pointer;

  isc_blob_handle = Pointer;
  isc_db_handle = Pointer;
  isc_form_handle = Pointer;
  isc_req_handle = Pointer;
  isc_stmt_handle = Pointer;
  isc_svc_handle = Pointer;
  isc_tr_handle = Pointer;
  isc_win_handle = Pointer;
  isc_callback = Pointer;
  isc_resv_handle = ISC_LONG;

  {*************************}
  { OSRI database functions }
  {*************************}


  function isc_attach_database(var status_vector : ISC_STATUS;
                db_name_length : short;
                db_name : PAnsiChar;
                var db_handle : isc_db_handle;
                parm_buffer_length : short;
                parm_buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_array_get_slice(var status_vector : ISC_STATUS; 
                var db_handle : isc_db_handle;
                var trans_handle : isc_tr_handle;
                var array_id : ISC_QUAD;
                var desc : ISC_ARRAY_DESC;
                dest_array : Pointer;
                var slice_length : ISC_LONG) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_array_get_slice2(var status_vector : ISC_STATUS; 
                var db_handle : isc_db_handle;
                var trans_handle : isc_tr_handle;
                var array_id : ISC_QUAD;
                var desc : ISC_ARRAY_DESC_V2;
                dest_array : Pointer;
                var slice_length : ISC_LONG) : ISC_STATUS; cdecl; external gds32dll;

  function isc_array_lookup_bounds(var status_vector : ISC_STATUS; 
              var db_handle : isc_db_handle; 
              var trans_handle : isc_tr_handle;
              table_name : PAnsiChar;
              column_name : PAnsiChar;
              var desc : ISC_ARRAY_DESC) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_array_lookup_bounds2(var status_vector : ISC_STATUS; 
              var db_handle : isc_db_handle; 
              var trans_handle : isc_tr_handle; 
              table_name : PAnsiChar;
              column_name : PAnsiChar;
              var desc : ISC_ARRAY_DESC_V2) : ISC_STATUS; cdecl; external gds32dll;

  function isc_array_lookup_desc(var status_vector : ISC_STATUS; 
                  var db_handle : isc_db_handle;
                  var trans_handle : isc_tr_handle;
                  table_name : PAnsiChar;
                  column_name : PAnsiChar;
                  var desc : ISC_ARRAY_DESC) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_array_lookup_desc2(var status_vector : ISC_STATUS;
                  var db_handle : isc_db_handle;
                  var trans_handle : isc_tr_handle;
                  table_name : PAnsiChar;
                  column_name : PAnsiChar;
                  var desc : ISC_ARRAY_DESC_V2) : ISC_STATUS; cdecl; external gds32dll;

  function isc_array_set_desc(var status_vector : ISC_STATUS; 
               table_name : PAnsiChar;
               column_name : PAnsiChar;
               var sql_dtype : short;
               var sql_length : short;
               var dimensions : short;
               desc : ISC_ARRAY_DESC) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_array_set_desc2(var status_vector : ISC_STATUS; 
               table_name : PAnsiChar;
               column_name : PAnsiChar;
               var sql_dtype : short;
               var sql_length : short;
               var dimensions : short;
               desc : ISC_ARRAY_DESC_V2) : ISC_STATUS; cdecl; external gds32dll;

  function isc_array_put_slice(var status_vector : ISC_STATUS; 
                var db_handle : isc_db_handle; 
                var trans_handle : isc_tr_handle; 
                var array_id : ISC_QUAD;
                var desc : ISC_ARRAY_DESC;
                source_array : Pointer;
                var slice_length : ISC_LONG) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_array_put_slice2(var status_vector : ISC_STATUS;
                var db_handle : isc_db_handle; 
                var trans_handle : isc_tr_handle; 
                var array_id : ISC_QUAD;
                var desc : ISC_ARRAY_DESC_V2;
                source_array : Pointer;
                var slice_length : ISC_LONG) : ISC_STATUS; cdecl; external gds32dll;

  procedure isc_blob_default_desc(var desc : ISC_BLOB_DESC;
                                  table_name : PUnsigned_Char;
                                  column_name : PUnsigned_Char); cdecl; external gds32dll; deprecated;

  procedure isc_blob_default_desc2(var desc : ISC_BLOB_DESC_V2;
                                   table_name : PChar;
                                   column_name : PChar); cdecl; external gds32dll;

  function isc_blob_gen_bpb(var status_vector : ISC_STATUS;
            var to_desc : ISC_BLOB_DESC;
            var from_desc : ISC_BLOB_DESC;
            bpb_buffer_length : unsigned_short;
            var bpb_buffer : unsigned_char;
            var bpb_length : unsigned_short) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_blob_gen_bpb2(var status_vector : ISC_STATUS;
            var to_desc : ISC_BLOB_DESC_V2;
            var from_desc : ISC_BLOB_DESC_V2;
            bpb_buffer_length : unsigned_short;
            var bpb_buffer : unsigned_char;
            var bpb_length : unsigned_short) : ISC_STATUS; cdecl; external gds32dll;

  function isc_blob_info(var status_vector : ISC_STATUS; 
                var blob_handle : isc_blob_handle;
                item_list_buffer_length : short;
                item_list_buffer : PAnsiChar;
                result_buffer_length : short; 
                var result_buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_blob_lookup_desc(var status_vector : ISC_STATUS;
                var db_handle : isc_db_handle;
                var trans_handle : isc_tr_handle;
                table_name : Punsigned_char;
                column_name : Punsigned_char;
                var desc : ISC_BLOB_DESC;
                global : Punsigned_char) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_blob_lookup_desc2(var status_vector : ISC_STATUS;
                var db_handle : isc_db_handle;
                var trans_handle : isc_tr_handle;
                table_name : Punsigned_char;
                column_name : Punsigned_char;
                var desc : ISC_BLOB_DESC_V2;
                global : Punsigned_char) : ISC_STATUS; cdecl; external gds32dll;

  function isc_blob_set_desc(var status_vector : ISC_STATUS;
             table_name : Punsigned_char;
             column_name : Punsigned_char;
             subtype : short;
             charset : short;
             segment_size : short;
             var desc : ISC_BLOB_DESC) : ISC_STATUS; cdecl; external gds32dll; deprecated;

  function isc_blob_set_desc2(var status_vector : ISC_STATUS;
             table_name : Punsigned_char;
             column_name : Punsigned_char;
             subtype : short;
             charset : short;
             segment_size : short;
             var desc : ISC_BLOB_DESC_V2) : ISC_STATUS; cdecl; external gds32dll;

  function isc_cancel_blob(var status_vector : ISC_STATUS; 
                  var blob_handle : isc_blob_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_cancel_events(var status_vector : ISC_STATUS; 
              var db_handle : isc_db_handle; 
              var event_id : ISC_LONG) : ISC_STATUS; cdecl; external gds32dll;

  function isc_close_blob(var status_vector : ISC_STATUS; 
                 var blob_handle : isc_blob_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_commit_retaining(var status_vector : ISC_STATUS; 
                 var trans_handle : isc_tr_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_commit_transaction(var status_vector : ISC_STATUS; 
                   var trans_handle : isc_tr_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_create_blob2(var status_vector : ISC_STATUS; 
             var db_handle : isc_db_handle; 
             var trans_handle : isc_tr_handle; 
             var blob_handle : isc_blob_handle;
             var blob_id : ISC_QUAD;
             bpb_length : short;
             bpb_adress : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;


  function isc_database_info(var status_vector : ISC_STATUS; 
              var db_handle : isc_db_handle;
              item_list_buffer_length : short;
              item_list_buffer : PAnsiChar;
              result_buffer_length : short;
              result_buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  procedure isc_decode_sql_date (var ib_date : ISC_DATE;
            var tm_date : tm); cdecl; external gds32dll;

  procedure isc_decode_sql_time (var ib_time : ISC_TIME;
            var tm_date : tm); cdecl; external gds32dll;

  procedure isc_decode_date (var ib_timestamp : ISC_TIMESTAMP;
            var tm_date : tm); cdecl; external gds32dll;

  procedure isc_decode_timestamp (var ib_timestamp : ISC_TIMESTAMP;
            var tm_date : tm); cdecl; external gds32dll;

  function isc_detach_database(var status_vector : ISC_STATUS;  
                var db_handle : isc_db_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_drop_database(var status_vector : ISC_STATUS;  
              var db_handle : isc_db_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_allocate_statement(var status_vector : ISC_STATUS; 
                  var db_handle : isc_db_handle; 
                  var stmt_handle : isc_stmt_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_alloc_statement2(var status_vector : ISC_STATUS; 
                var db_handle : isc_db_handle; 
                var stmt_handle :  isc_stmt_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_describe(var status_vector : ISC_STATUS; 
              var stmt_handle : isc_stmt_handle;
              da_version : unsigned_short;
              var xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_describe_bind(var status_vector : ISC_STATUS;
                   var stmt_handle : isc_stmt_handle;
                   da_version : unsigned_short;
                   var xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_exec_immed2(var status_vector : ISC_STATUS; 
                 var db_handle : isc_db_handle; 
                 var trans_handle : isc_tr_handle; 
                 length : unsigned_short;
                 statement : PAnsiChar;
                 dialect : unsigned_short;
                 var in_xsqlda : XSQLDA;
                 var out_xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_execute(var status_vector : ISC_STATUS; 
             var trans_handle : isc_tr_handle;
             var stmt_handle : isc_stmt_handle;
             da_version : unsigned_short;
             var xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_execute2(var status_vector : ISC_STATUS; 
              var trans_handle : isc_tr_handle;
              var stmt_handle : isc_stmt_handle;
              da_version : unsigned_short;
              var in_xsqlda : XSQLDA;
              var out_xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_fetch(var status_vector : ISC_STATUS; 
                 var stmt_handle : isc_stmt_handle;
                 da_version : unsigned_short;
                 var xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_free_statement(var status_vector : ISC_STATUS; 
              var stmt_handle : isc_stmt_handle;
              option : unsigned_short) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_prepare(var status_vector : ISC_STATUS; 
             var trans_handle : isc_tr_handle; 
             var stmt_handle : isc_stmt_handle;
             length : unsigned_short;
             statement : PAnsiChar;
             dialect : unsigned_short;
             var xsqlda : XSQLDA) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_set_cursor_name(var status_vector : ISC_STATUS; 
               var stmt_handle : isc_stmt_handle;
               cursor_name : PAnsiChar;
               _type : unsigned_short) : ISC_STATUS; cdecl; external gds32dll;

  function isc_dsql_sql_info(var status_vector : ISC_STATUS;
              var stmt_handle : isc_stmt_handle;
              item_length : short;
              items : PAnsiChar;
              buffer_length : short;
              buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  procedure isc_encode_sql_date (var tm_date : tm;
            var ib_date : ISC_DATE); cdecl; external gds32dll;

  procedure isc_encode_sql_time (var tm_date : tm;
            var ib_time : ISC_TIME); cdecl; external gds32dll;

  procedure isc_encode_timestamp (var tm_date : tm;
            var ib_timestamp : ISC_TIMESTAMP); cdecl; external gds32dll;

  function isc_event_block (var event_buffer : PAnsiChar;
                   var result_buffer : PAnsiChar;
                   id_count : unsigned_short;
                   names : array of PAnsiChar) : ISC_LONG; cdecl; external gds32dll;

  procedure isc_event_counts (var status_vector : ISC_LONG; 
             buffer_lengh : short;
             event_buffer : PAnsiChar;
             result_buffer : PAnsiChar);  cdecl; external gds32dll;

  procedure isc_expand_dpb (var dpb : PAnsiChar;
                  var dpb_size : short;
                  items : array of PAnsiChar);  cdecl; external gds32dll;

  function isc_get_segment(var status_vector : ISC_STATUS;
                  var blob_handle : isc_blob_handle;
                  var actual_seg_length : unsigned_short;
                  seg_buffer_length : unsigned_short;
                  seg_buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_interprete (buffer : PAnsiChar;
                 var status_vector : PISC_STATUS) : ISC_STATUS; cdecl; external gds32dll;

  function isc_open_blob2(var status_vector : ISC_STATUS; 
                 var db_handle : isc_db_handle; 
                 var trans_handle : isc_tr_handle;
                 var blob_handle : isc_blob_handle;
                 var blob_id : ISC_QUAD;
                 bpb_length : short;
                 bpb_address : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_prepare_transaction2(var status_vector : ISC_STATUS; 
               var trans_handle : isc_tr_handle; 
               msg_length : short;
               _message : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  procedure isc_print_sqlerror (SQLCODE : short;
               var status_vector : ISC_STATUS); cdecl; external gds32dll;

  function isc_print_status (var status_vector : ISC_STATUS) : ISC_STATUS; cdecl; external gds32dll;

  function isc_put_segment(var status_vector : ISC_STATUS; 
            var blob_handle : isc_blob_handle;
            seg_buffer_length : unsigned_short;
            seg_buffer_address : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_que_events(var status_vector : ISC_STATUS; 
                 var db_handle : isc_db_handle; 
                 var event_id : ISC_LONG;
                 length : short;
                 event_buffer : PAnsiChar;
                 event_function : isc_callback;
                 event_function_arg : Pointer) : ISC_STATUS; cdecl; external gds32dll;

  function isc_rollback_retaining(var status_vector : ISC_STATUS; 
               var trans_handle : isc_tr_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_rollback_transaction(var status_vector : ISC_STATUS; 
               var trans_handle : isc_tr_handle) : ISC_STATUS; cdecl; external gds32dll;

  function isc_start_multiple(var status_vector : ISC_STATUS; 
               var trans_handle : isc_tr_handle;
               db_handle_count : short;
               teb_vector_address : Pointer) : ISC_STATUS; cdecl; external gds32dll;

  function isc_start_transaction(var status_vector : ISC_STATUS;
                   var trans_handle : isc_tr_handle;
                   db_handle_count : short;
                   var db_handle : isc_db_handle;
                   tpb_length : unsigned_short;
                   tpb_address : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_sqlcode (var status_vector : ISC_STATUS) : ISC_LONG; cdecl; external gds32dll;

  procedure isc_sql_interprete (SQLCODE : short;
               buffer : PAnsiChar;
               buffer_length : short); cdecl; external gds32dll;

  function isc_transaction_info(var status_vector : ISC_STATUS;  
                 var trans_handle : isc_tr_handle; 
                 item_list_buffer_length : short;
                 item_list_buffer : PAnsiChar;
                 result_buffer_length : short;
                 result_buffer : PAnsiChar) : ISC_STATUS; cdecl; external gds32dll;

  function isc_vax_integer(buffer : PAnsiChar;
            length : short) : ISC_LONG; cdecl; external gds32dll; deprecated;

  function isc_portable_integer(buffer : PAnsiChar;
                                length : short) : ISC_INT64; cdecl; external gds32dll;

  {***********************************}
  { Security Functions and structures }
  {***********************************}

const
  sec_uid_spec		    = $01;
  sec_gid_spec		    = $02;
  sec_server_spec		    = $04;
  sec_password_spec	    = $08;
  sec_group_name_spec	    = $10;
  sec_first_name_spec	    = $20;
  sec_middle_name_spec        = $40;
  sec_last_name_spec	    = $80;
  sec_dba_user_name_spec      = $100;
  sec_dba_password_spec       = $200;

  sec_protocol_tcpip            = 1;
  sec_protocol_netbeui          = 2;
  sec_protocol_spx              = 3;
  sec_protocol_local            = 4;

type
  USER_SEC_DATA = packed record
    sec_flags : short;		     { which fields are specified }
    uid : int;			     { the user's id }
    gif : int;			     { the user's group id }
    protocol : int;		     { protocol to use for connection }
    server : PAnsiChar;          { server to administer }
    user_name : PAnsiChar;       { the user's name }
    password : PAnsiChar;        { the user's password }
    group_name : PAnsiChar;      { the group name }
    first_name : PAnsiChar;	     { the user's first name }
    middle_name : PAnsiChar;     { the user's middle name }
    last_name : PAnsiChar;	     { the user's last name }
    dba_user_name : PAnsiChar;   { the dba user name }
    dba_password : PAnsiChar;    { the dba password }
  end;
  PUSER_SEC_DATA = ^USER_SEC_DATA;

  function isc_add_user(var status_vector : ISC_STATUS; var _user_sec_data : USER_SEC_DATA) : int; cdecl; external gds32dll;

  function isc_delete_user(var status_vector : ISC_STATUS; var _user_sec_data : USER_SEC_DATA) : int; cdecl; external gds32dll;

  function isc_modify_user(var status_vector : ISC_STATUS; var _user_sec_data : USER_SEC_DATA) : int; cdecl; external gds32dll;

  {********************************}
  {  Other OSRI functions          }
  {********************************}

  {***************************}
  { Other Sql functions       }
  {***************************}

  {***********************************}
  { Other Dynamic sql functions       }
  {***********************************}

  {****************************}
  { Other Blob functions       }
  {****************************}

  function BLOB_put(c : AnsiChar;
				 var blob_stream : BSTREAM) : Int; cdecl; external gds32dll;

  function BLOB_get(var blob_stream : BSTREAM) : Int; cdecl; external gds32dll;

  {****************************}
  { Other Misc functions       }
  {****************************}

  {***************************************}
  { Service manager functions             }
  {***************************************}

  {******************************}
  { Client information functions }
  {******************************}
  procedure isc_get_client_version (buffer : PAnsiChar); cdecl; external gds32dll;
  function isc_get_client_major_version() : Int; cdecl; external gds32dll;
  function isc_get_client_minor_version() : Int; cdecl; external gds32dll;

  {*****************************}
  { Forms functions             }
  {*****************************}

  {*************************************************}
  { Actions to pass to the blob filter (ctl_source) }
  {*************************************************}

const
  isc_blob_filter_open             = 0;
  isc_blob_filter_get_segment      = 1;
  isc_blob_filter_close            = 2;
  isc_blob_filter_create           = 3;
  isc_blob_filter_put_segment      = 4;
  isc_blob_filter_alloc            = 5;
  isc_blob_filter_free             = 6;
  isc_blob_filter_seek             = 7;

  { START CONVERT TAG }
  {*****************}
  { Blr definitions }
  {*****************}
  { END CONVERT TAG }


  { START CONVERT TAG }
  blr_text                           = 14;
  blr_text2                          = 15;
  blr_short                          = 7;
  blr_long                           = 8;
  blr_quad                           = 9;
  blr_int64                          = 16;
  blr_float                          = 10;
  blr_double                         = 27;
  blr_d_float                        = 11;
  blr_timestamp                      = 35;
  blr_varying                        = 37;
  blr_varying2                       = 38;
  blr_blob                           = 261;
  blr_cstring                        = 40;
  blr_cstring2                       = 41;
  blr_blob_id                        = 45;
  blr_sql_date                       = 12;
  blr_sql_time                       = 13;
  blr_boolean_dtype                        = 17;


  { Historical alias for pre V6 applications }
  blr_date                           = blr_timestamp;

  blr_inner                          = 0;
  blr_left                           = 1;
  blr_right                          = 2;
  blr_full                           = 3;

  blr_gds_code                       = 0;
  blr_sql_code                       = 1;
  blr_exception                      = 2;
  blr_trigger_code                   = 3;
  blr_default_code                   = 4;

  blr_version4                       = 4;
  blr_version5                       = 5;
  blr_eoc                            = 76;
  blr_end                            = 255;

  blr_assignment                     = 1;
  blr_begin                          = 2;
  blr_dcl_variable                   = 3;
  blr_message                        = 4;
  blr_erase                          = 5;
  blr_fetch                          = 6;
  blr_for                            = 7;
  blr_if                             = 8;
  blr_loop                           = 9;
  blr_modify                         = 10;
  blr_handler                        = 11;
  blr_receive                        = 12;
  blr_select                         = 13;
  blr_send                           = 14;
  blr_store                          = 15;
  blr_label                          = 17;
  blr_leave                          = 18;
  blr_store2                         = 19;
  blr_post                           = 20;

  blr_literal                        = 21;
  blr_dbkey                          = 22;
  blr_field                          = 23;
  blr_fid                            = 24;
  blr_parameter                      = 25;
  blr_variable                       = 26;
  blr_average                        = 27;
  blr_count                          = 28;
  blr_maximum                        = 29;
  blr_minimum                        = 30;
  blr_total                          = 31;
  blr_add                            = 34;
  blr_subtract                       = 35;
  blr_multiply                       = 36;
  blr_divide                         = 37;
  blr_negate                         = 38;
  blr_concatenate                    = 39;
  blr_substring                      = 40;
  blr_parameter2                     = 41;
  blr_from                           = 42;
  blr_via                            = 43;
  blr_user_name                      = 44;
  blr_null                           = 45;

  blr_eql                            = 47;
  blr_neq                            = 48;
  blr_gtr                            = 49;
  blr_geq                            = 50;
  blr_lss                            = 51;
  blr_leq                            = 52;
  blr_containing                     = 53;
  blr_matching                       = 54;
  blr_starting                       = 55;
  blr_between                        = 56;
  blr_or                             = 57;
  blr_and                            = 58;
  blr_not                            = 59;
  blr_any                            = 60;
  blr_missing                        = 61;
  blr_unique                         = 62;
  blr_like                           = 63;

  blr_stream                         = 65;
  blr_set_index                      = 66;
  blr_rse                            = 67;
  blr_first                          = 68;
  blr_project                        = 69;
  blr_sort                           = 70;
  blr_boolean                        = 71;
  blr_ascending                      = 72;
  blr_descending                     = 73;
  blr_relation                       = 74;
  blr_rid                            = 75;
  blr_union                          = 76;
  blr_map                            = 77;
  blr_group_by                       = 78;
  blr_aggregate                      = 79;
  blr_join_type                      = 80;
  blr_rows                           = 81;

  { sub parameters for blr_rows }

  blr_ties                           = 0;
  blr_percent			   = 1;

  blr_agg_count                      = 83;
  blr_agg_max                        = 84;
  blr_agg_min                        = 85;
  blr_agg_total                      = 86;
  blr_agg_average                    = 87;
  blr_parameter3                     = 88;
  blr_run_count                      = 118;
  blr_run_max                        = 89;
  blr_run_min                        = 90;
  blr_run_total                      = 91;
  blr_run_average                    = 92;
  blr_agg_count2                     = 93;
  blr_agg_count_distinct             = 94;
  blr_agg_total_distinct             = 95;
  blr_agg_average_distinct           = 96;

  blr_function                       = 100;
  blr_gen_id                         = 101;
  blr_prot_mask                      = 102;
  blr_upcase                         = 103;
  blr_lock_state                     = 104;
  blr_value_if                       = 105;
  blr_matching2                      = 106;
  blr_index                          = 107;
  blr_ansi_like                      = 108;
  blr_bookmark                       = 109;
  blr_crack                          = 110;
  blr_force_crack                    = 111;
  blr_seek                           = 112;
  blr_find                           = 113;

  blr_continue                       = 0;
  blr_forward                        = 1;
  blr_backward                       = 2;
  blr_bof_forward                    = 3;
  blr_eof_backward                   = 4;

  blr_lock_relation                  = 114;
  blr_lock_record                    = 115;
  blr_set_bookmark		   = 116;
  blr_get_bookmark		   = 117;
  blr_rs_stream                      = 119;
  blr_exec_proc                      = 120;
  blr_begin_range                    = 121;
  blr_end_range                      = 122;
  blr_delete_range                   = 123;
  blr_procedure                      = 124;
  blr_pid                            = 125;
  blr_exec_pid                       = 126;
  blr_singular                       = 127;
  blr_abort                          = 128;
  blr_block                          = 129;
  blr_error_handler                  = 130;
  blr_cast                           = 131;
  blr_release_lock                   = 132;
  blr_release_locks                  = 133;
  blr_start_savepoint                = 134;
  blr_end_savepoint                  = 135;
  blr_find_dbkey                     = 136;
  blr_range_relation                 = 137;
  blr_delete_ranges                  = 138;

  blr_plan                           = 139;
  blr_merge                          = 140;
  blr_join                           = 141;
  blr_sequential                     = 142;
  blr_navigational                   = 143;
  blr_indices                        = 144;
  blr_retrieve                       = 145;

  blr_relation2                      = 146;
  blr_rid2                           = 147;
  blr_reset_stream                   = 148;
  blr_release_bookmark               = 149;
  blr_set_generator                  = 150;
  blr_ansi_any			   = 151;
  blr_exists			   = 152;
  blr_cardinality			   = 153;

  blr_record_version		   = 154		{ get tid of record };
  blr_stall			   = 155		{ fake server stall };
  blr_seek_no_warn		   = 156;
  blr_find_dbkey_version		   = 157;
  blr_ansi_all			   = 158;

  blr_extract                        = 159;

  { sub parameters for blr_extract }

  blr_extract_year                   = 0;
  blr_extract_month                  = 1;
  blr_extract_day	                   = 2;
  blr_extract_hour                   = 3;
  blr_extract_minute                 = 4;
  blr_extract_second                 = 5;
  blr_extract_weekday                = 6;
  blr_extract_yearday                = 7;

  blr_current_date                   = 160;
  blr_current_timestamp              = 161;
  blr_current_time                   = 162;

  { These verbs were added in 6.0, primarily to support 64-bit integers }

  blr_add2	          = 163;
  blr_subtract2	          = 164;
  blr_multiply2             = 165;
  blr_divide2	          = 166;
  blr_agg_total2            = 167;
  blr_agg_total_distinct2   = 168;
  blr_agg_average2          = 169;
  blr_agg_average_distinct2 = 170;
  blr_average2		  = 171;
  blr_gen_id2		  = 172;
  blr_set_generator2        = 173;

  { These verbs were added in 7.0 for BOOLEAN dtype supprt }
  blr_boolean_true          = 174;
  blr_boolean_false         = 175;

  { These verbs were added in 7.1 for SQL savepoint support }
  blr_start_savepoint2      = 176;
  blr_release_savepoint     = 177;
  blr_rollback_savepoint    = 178;

  {********************************}
  { Database parameter block stuff }
  {********************************}

const
  isc_dpb_version1                  = 1;
  isc_dpb_cdd_pathname              = 1;
  isc_dpb_allocation                = 2;
  isc_dpb_journal                   = 3;
  isc_dpb_page_size                 = 4;
  isc_dpb_num_buffers               = 5;
  isc_dpb_buffer_length             = 6;
  isc_dpb_debug                     = 7;
  isc_dpb_garbage_collect           = 8;
  isc_dpb_verify                    = 9;
  isc_dpb_sweep                     = 10;
  isc_dpb_enable_journal            = 11;
  isc_dpb_disable_journal           = 12;
  isc_dpb_dbkey_scope               = 13;
  isc_dpb_number_of_users           = 14;
  isc_dpb_trace                     = 15;
  isc_dpb_no_garbage_collect        = 16;
  isc_dpb_damaged                   = 17;
  isc_dpb_license                   = 18;
  isc_dpb_sys_user_name             = 19;
  isc_dpb_encrypt_key               = 20;
  isc_dpb_activate_shadow           = 21;
  isc_dpb_sweep_interval            = 22;
  isc_dpb_delete_shadow             = 23;
  isc_dpb_force_write               = 24;
  isc_dpb_begin_log                 = 25;
  isc_dpb_quit_log                  = 26;
  isc_dpb_no_reserve                = 27;
  isc_dpb_user_name                 = 28;
  isc_dpb_password                  = 29;
  isc_dpb_password_enc              = 30;
  isc_dpb_sys_user_name_enc         = 31;
  isc_dpb_interp                    = 32;
  isc_dpb_online_dump               = 33;
  isc_dpb_old_file_size             = 34;
  isc_dpb_old_num_files             = 35;
  isc_dpb_old_file_name             = 36;
  isc_dpb_old_start_page            = 37;
  isc_dpb_old_start_seqno           = 38;
  isc_dpb_old_start_file            = 39;
  isc_dpb_drop_walfile              = 40;
  isc_dpb_old_dump_id               = 41;
  isc_dpb_wal_backup_dir            = 42;
  isc_dpb_wal_chkptlen              = 43;
  isc_dpb_wal_numbufs               = 44;
  isc_dpb_wal_bufsize               = 45;
  isc_dpb_wal_grp_cmt_wait          = 46;
  isc_dpb_lc_messages               = 47;
  isc_dpb_lc_ctype                  = 48;
  isc_dpb_cache_manager		  = 49;
  isc_dpb_shutdown		  = 50;
  isc_dpb_online			  = 51;
  isc_dpb_shutdown_delay		  = 52;
  isc_dpb_reserved		  = 53;
  isc_dpb_overwrite		  = 54;
  isc_dpb_sec_attach		  = 55;
  isc_dpb_disable_wal		  = 56;
  isc_dpb_connect_timeout           = 57;
  isc_dpb_dummy_packet_interval     = 58;
  isc_dpb_gbak_attach               = 59;
  isc_dpb_sql_role_name             = 60;
  isc_dpb_set_page_buffers          = 61;
  isc_dpb_working_directory         = 62;
  isc_dpb_sql_dialect               = 63;
  isc_dpb_set_db_readonly           = 64;
  isc_dpb_set_db_sql_dialect        = 65;
  isc_dpb_gfix_attach		  = 66;
  isc_dpb_gstat_attach		  = 67;
  isc_dpb_gbak_ods_version          = 68;
  isc_dpb_gbak_ods_minor_version    = 69;
  isc_dpb_set_group_commit	  = 70;
  isc_dpb_gbak_validate             = 71;
  isc_dpb_client_interbase_var	  = 72;
  isc_dpb_admin_option              = 73;
  isc_dpb_flush_interval            = 74;
  isc_dpb_instance_name	  	  = 75;
  isc_dpb_old_overwrite             = 76;
  isc_dpb_archive_database          = 77;
  isc_dpb_archive_journals          = 78;
  isc_dpb_archive_sweep             = 79;
  isc_dpb_archive_dumps             = 80;
  isc_dpb_archive_recover           = 81;
  isc_dpb_recover_until             = 82;
  isc_dpb_force                     = 83;

  {*******************************}
  { isc_dpb_verify specific flags }
  {*******************************}

  isc_dpb_pages                     = 1;
  isc_dpb_records                   = 2;
  isc_dpb_indices                   = 4;
  isc_dpb_transactions              = 8;
  isc_dpb_no_update                 = 16;
  isc_dpb_repair                    = 32;
  isc_dpb_ignore                    = 64;

  {*********************************}
  { isc_dpb_shutdown specific flags }
  {*********************************}

  isc_dpb_shut_cache               = 1;
  isc_dpb_shut_attachment          = 2;
  isc_dpb_shut_transaction         = 4;
  isc_dpb_shut_force               = 8;

  {************************************}
  { Bit assignments in RDB$SYSTEM_FLAG }
  {************************************}

  RDB_system                         = 1;
  RDB_id_assigned                    = 2;
  
  {***********************************}
  { Transaction parameter block stuff }
  {***********************************}

  isc_tpb_version1                  = 1;
  isc_tpb_version3                  = 3;
  isc_tpb_consistency               = 1;
  isc_tpb_concurrency               = 2;
  isc_tpb_shared                    = 3;
  isc_tpb_protected                 = 4;
  isc_tpb_exclusive                 = 5;
  isc_tpb_wait                      = 6;
  isc_tpb_nowait                    = 7;
  isc_tpb_read                      = 8;
  isc_tpb_write                     = 9;
  isc_tpb_lock_read                 = 10;
  isc_tpb_lock_write                = 11;
  isc_tpb_verb_time                 = 12;
  isc_tpb_commit_time               = 13;
  isc_tpb_ignore_limbo              = 14;
  isc_tpb_read_committed		  = 15;
  isc_tpb_autocommit		  = 16;
  isc_tpb_rec_version		  = 17;
  isc_tpb_no_rec_version		  = 18;
  isc_tpb_restart_requests	  = 19;
  isc_tpb_no_auto_undo              = 20;
  isc_tpb_no_savepoint              = 21;
  
  {**********************}
  { Blob Parameter Block }
  {**********************}

  isc_bpb_version1                  = 1;
  isc_bpb_source_type               = 1;
  isc_bpb_target_type               = 2;
  isc_bpb_type                      = 3;
  isc_bpb_source_interp             = 4;
  isc_bpb_target_interp             = 5;
  isc_bpb_filter_parameter          = 6;

  isc_bpb_type_segmented            = 0;
  isc_bpb_type_stream               = 1;
  
  {*******************************}
  { Service parameter block stuff }
  {*******************************}

  isc_spb_version1                  = 1;
  isc_spb_current_version           = 2;
  isc_spb_version			  = isc_spb_current_version;
  isc_spb_user_name                 = isc_dpb_user_name;
  isc_spb_sys_user_name             = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc         = isc_dpb_sys_user_name_enc;
  isc_spb_password                  = isc_dpb_password;
  isc_spb_password_enc              = isc_dpb_password_enc;
  isc_spb_command_line              = 105;
  isc_spb_dbname                    = 106;
  isc_spb_verbose                   = 107;
  isc_spb_options                   = 108;
  isc_spb_user_dbname               = 109;

  isc_spb_connect_timeout           = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval     = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name             = isc_dpb_sql_role_name;
  isc_spb_instance_name             = isc_dpb_instance_name;
  
  {*******************************}
  { Information call declarations }
  {*******************************}

  {**************************}
  { Common, structural codes }
  {**************************}

  isc_info_end                      = 1;
  isc_info_truncated                = 2;
  isc_info_error                    = 3;
  isc_info_data_not_ready	          = 4;
  isc_info_flag_end		  = 127;

  {****************************}
  { Database information items }
  {****************************}

  isc_info_db_id                    = 4;
  isc_info_reads                    = 5;
  isc_info_writes                   = 6;
  isc_info_fetches                  = 7;
  isc_info_marks                    = 8;
  isc_info_implementation           = 11;
  isc_info_version                  = 12;
  isc_info_base_level               = 13;
  isc_info_svr_maj_ver              = isc_info_base_level;
  isc_info_page_size                = 14;
  isc_info_num_buffers              = 15;
  isc_info_limbo                    = 16;
  isc_info_current_memory           = 17;
  isc_info_max_memory               = 18;
  isc_info_window_turns             = 19;
  isc_info_license                  = 20;
  isc_info_allocation               = 21;
  isc_info_attachment_id            = 22;
  isc_info_read_seq_count           = 23;
  isc_info_read_idx_count           = 24;
  isc_info_insert_count             = 25;
  isc_info_update_count             = 26;
  isc_info_delete_count             = 27;
  isc_info_backout_count            = 28;
  isc_info_purge_count              = 29;
  isc_info_expunge_count            = 30;
  isc_info_sweep_interval           = 31;
  isc_info_ods_version              = 32;
  isc_info_ods_minor_version        = 33;
  isc_info_no_reserve               = 34;
  isc_info_logfile                  = 35;
  isc_info_cur_logfile_name         = 36;
  isc_info_cur_log_part_offset      = 37;
  isc_info_num_wal_buffers          = 38;
  isc_info_wal_buffer_size          = 39;
  isc_info_wal_ckpt_length          = 40;
  isc_info_wal_cur_ckpt_length      = 41;
  isc_info_wal_prv_ckpt_fname       = 42;
  isc_info_wal_prv_ckpt_poffset     = 43;
  isc_info_wal_recv_ckpt_fname      = 44;
  isc_info_wal_recv_ckpt_poffset    = 45;
  isc_info_wal_ckpt_interval        = 47;
  isc_info_wal_num_io               = 48;
  isc_info_wal_avg_io_size          = 49;
  isc_info_wal_num_commits          = 50;
  isc_info_wal_avg_grpc_size        = 51;
  isc_info_forced_writes		  = 52;
  isc_info_user_names		  = 53;
  isc_info_page_errors		  = 54;
  isc_info_record_errors		  = 55;
  isc_info_bpage_errors		  = 56;
  isc_info_dpage_errors	  	  = 57;
  isc_info_ipage_errors	  	  = 58;
  isc_info_ppage_errors		  = 59;
  isc_info_tpage_errors	  	  = 60;
  isc_info_set_page_buffers         = 61;
  isc_info_db_sql_dialect           = 62;
  isc_info_db_read_only             = 63;
  isc_info_db_size_in_pages	  = 64;
  isc_info_db_reads                 = 65;
  isc_info_db_writes                = 66;
  isc_info_db_fetches               = 67;
  isc_info_db_marks                 = 68;
  isc_info_db_group_commit          = 69;
  isc_info_att_charset              = 70;
  isc_info_svr_min_ver              = 71;
  isc_info_ib_env_var               = 72;
  isc_info_server_tcp_port          = 73;

  {************************************}
  { Database information return values }
  {************************************}

  isc_info_db_impl_rdb_vms          = 1;
  isc_info_db_impl_rdb_eln          = 2;
  isc_info_db_impl_rdb_eln_dev      = 3;
  isc_info_db_impl_rdb_vms_y        = 4;
  isc_info_db_impl_rdb_eln_y        = 5;
  isc_info_db_impl_jri              = 6;
  isc_info_db_impl_jsv              = 7;
  isc_info_db_impl_isc_a            = 25;
  isc_info_db_impl_isc_u            = 26;
  isc_info_db_impl_isc_v            = 27;
  isc_info_db_impl_isc_s            = 28;
  isc_info_db_impl_isc_apl_68K      = 25;
  isc_info_db_impl_isc_vax_ultr     = 26;
  isc_info_db_impl_isc_vms          = 27;
  isc_info_db_impl_isc_sun_68k      = 28;
  isc_info_db_impl_isc_os2          = 29;
  isc_info_db_impl_isc_sun4         = 30;
  isc_info_db_impl_isc_hp_ux        = 31;
  isc_info_db_impl_isc_sun_386i     = 32;
  isc_info_db_impl_isc_vms_orcl     = 33;
  isc_info_db_impl_isc_mac_aux      = 34;
  isc_info_db_impl_isc_rt_aix       = 35;
  isc_info_db_impl_isc_mips_ult     = 36;
  isc_info_db_impl_isc_xenix        = 37;
  isc_info_db_impl_isc_dg           = 38;
  isc_info_db_impl_isc_hp_mpexl     = 39;
  isc_info_db_impl_isc_hp_ux68K     = 40;
  isc_info_db_impl_isc_sgi          = 41;
  isc_info_db_impl_isc_sco_unix     = 42;
  isc_info_db_impl_isc_cray         = 43;
  isc_info_db_impl_isc_imp          = 44;
  isc_info_db_impl_isc_delta        = 45;
  isc_info_db_impl_isc_next         = 46;
  isc_info_db_impl_isc_dos          = 47;
  isc_info_db_impl_isc_winnt        = 48;
  isc_info_db_impl_isc_epson        = 49;

  isc_info_db_class_access          = 1;
  isc_info_db_class_y_valve         = 2;
  isc_info_db_class_rem_int         = 3;
  isc_info_db_class_rem_srvr        = 4;
  isc_info_db_class_pipe_int        = 7;
  isc_info_db_class_pipe_srvr       = 8;
  isc_info_db_class_sam_int         = 9;
  isc_info_db_class_sam_srvr        = 10;
  isc_info_db_class_gateway         = 11;
  isc_info_db_class_cache           = 12;

  {***************************}
  { Request information items }
  {***************************}

  isc_info_number_messages          = 4;
  isc_info_max_message              = 5;
  isc_info_max_send                 = 6;
  isc_info_max_receive              = 7;
  isc_info_state                    = 8;
  isc_info_message_number           = 9;
  isc_info_message_size             = 10;
  isc_info_request_cost             = 11;
  isc_info_access_path              = 12;
  isc_info_req_select_count         = 13;
  isc_info_req_insert_count         = 14;
  isc_info_req_update_count         = 15;
  isc_info_req_delete_count         = 16;
  
  {*******************}
  { Access path items }
  {*******************}

  isc_info_rsb_end		   = 0;
  isc_info_rsb_begin		   = 1;
  isc_info_rsb_type		   = 2;
  isc_info_rsb_relation		   = 3;
  isc_info_rsb_plan                  = 4;

  {***********}
  { Rsb types }
  {***********}

  isc_info_rsb_unknown		   = 1;
  isc_info_rsb_indexed		   = 2;
  isc_info_rsb_navigate		   = 3;
  isc_info_rsb_sequential	 	   = 4;
  isc_info_rsb_cross		   = 5;
  isc_info_rsb_sort		   = 6;
  isc_info_rsb_first		   = 7;
  isc_info_rsb_boolean		   = 8;
  isc_info_rsb_union		   = 9;
  isc_info_rsb_aggregate		  = 10;
  isc_info_rsb_merge		  = 11;
  isc_info_rsb_ext_sequential	  = 12;
  isc_info_rsb_ext_indexed	  = 13;
  isc_info_rsb_ext_dbkey		  = 14;
  isc_info_rsb_left_cross	 	  = 15;
  isc_info_rsb_select		  = 16;
  isc_info_rsb_sql_join		  = 17;
  isc_info_rsb_simulate		  = 18;
  isc_info_rsb_sim_cross		  = 19;
  isc_info_rsb_once		  = 20;
  isc_info_rsb_procedure		  = 21;

  {********************}
  { Bitmap expressions }
  {********************}

  isc_info_rsb_and		= 1;
  isc_info_rsb_or			= 2;
  isc_info_rsb_dbkey		= 3;
  isc_info_rsb_index		= 4;

  isc_info_req_active               = 2;
  isc_info_req_inactive             = 3;
  isc_info_req_send                 = 4;
  isc_info_req_receive              = 5;
  isc_info_req_select               = 6;
  isc_info_req_sql_stall		  = 7;

  {************************}
  { Blob information items }
  {************************}

  isc_info_blob_num_segments        = 4;
  isc_info_blob_max_segment         = 5;
  isc_info_blob_total_length        = 6;
  isc_info_blob_type                = 7;

  {*******************************}
  { Transaction information items }
  {*******************************}

  isc_info_tra_id                   = 4;

  {****************************
   * Service action items      *
   ****************************}

  { Range definitions for service actions.  Any action outside of
     this range is not supported }
  isc_action_min                 = 1;

  isc_action_svc_backup          = 1 { Starts database backup process on the server };
  isc_action_svc_restore         = 2 { Starts database restore process on the server };
  isc_action_svc_repair          = 3 { Starts database repair process on the server };
  isc_action_svc_add_user        = 4 { Adds a new user to the security database };
  isc_action_svc_delete_user     = 5 { Deletes a user record from the security database };
  isc_action_svc_modify_user     = 6 { Modifies a user record in the security database };
  isc_action_svc_display_user    = 7 { Displays a user record from the security database };
  isc_action_svc_properties      = 8 { Sets database properties };
  isc_action_svc_add_license     = 9 { Adds a license to the license file };
  isc_action_svc_remove_license = 10 { Removes a license from the license file };
  isc_action_svc_db_stats	      = 11 { Retrieves database statistics };
  isc_action_svc_get_ib_log     = 12 { Retrieves the InterBase log file from the server };
  isc_action_svc_add_db_alias     = 13 { Adds a new database alias };
  isc_action_svc_delete_db_alias  = 14 { Deletes an existing database alias };
  isc_action_svc_display_db_alias = 15 { Displays an existing database alias };

  isc_action_max                 = 16 { 1 more than above actions };

  {****************************
   * Service information items *
   ****************************}

  isc_info_svc_svr_db_info      = 50 { Retrieves the number of attachments and databases };
  isc_info_svc_get_license      = 51 { Retrieves all license keys and IDs from the license file };
  isc_info_svc_get_license_mask = 52 { Retrieves a bitmask representing licensed options on the server };
  isc_info_svc_get_config       = 53 { Retrieves the parameters and values for IB_CONFIG };
  isc_info_svc_version          = 54 { Retrieves the version of the services manager };
  isc_info_svc_server_version   = 55 { Retrieves the version of the InterBase server };
  isc_info_svc_implementation   = 56 { Retrieves the implementation of the InterBase server };
  isc_info_svc_capabilities     = 57 { Retrieves a bitmask representing the server's capabilities };
  isc_info_svc_user_dbpath      = 58 { Retrieves the path to the security database in use by the server };
  isc_info_svc_get_env	      = 59 { Retrieves the setting of $INTERBASE };
  isc_info_svc_get_env_lock     = 60 { Retrieves the setting of $INTERBASE_LCK };
  isc_info_svc_get_env_msg      = 61 { Retrieves the setting of $INTERBASE_MSG };
  isc_info_svc_line             = 62 { Retrieves 1 line of service output per call };
  isc_info_svc_to_eof           = 63 { Retrieves as much of the server output as will fit in the supplied buffer };
  isc_info_svc_timeout          = 64 { Sets / signifies a timeout value for reading service information };
  isc_info_svc_get_licensed_users = 65 { Retrieves the number of users licensed for accessing the server };
  isc_info_svc_limbo_trans	= 66 { Retrieve the limbo transactions };
  isc_info_svc_running		= 67 { Checks to see if a service is running on an attachment };
  isc_info_svc_get_users		= 68 { Returns the user information from isc_action_svc_display_users };
  isc_info_svc_get_db_alias	= 69 { Returns the database alias information from isc_action_svc_display_db_alias };

  {*****************************************************
   * Parameters for isc_action_{add|delete|modify)_user *
   *****************************************************}

  isc_spb_sec_userid            = 5;
  isc_spb_sec_groupid           = 6;
  isc_spb_sec_username          = 7;
  isc_spb_sec_password          = 8;
  isc_spb_sec_groupname         = 9;
  isc_spb_sec_firstname         = 10;
  isc_spb_sec_middlename        = 11;
  isc_spb_sec_lastname          = 12;

  {*****************************************************
   * Parameters for isc_action_{add|delete|display)_db_alias *
   *****************************************************}

  isc_spb_sec_db_alias_name     = 20;
  isc_spb_sec_db_alias_dbpath   = 21;

  {******************************************************
   * Parameters for isc_action_svc_(add|remove)_license, *
   * isc_info_svc_get_license                            *
   ******************************************************}

  isc_spb_lic_key               = 5;
  isc_spb_lic_id                = 6;
  isc_spb_lic_desc              = 7;


  {****************************************
   * Parameters for isc_action_svc_backup  *
   ****************************************}

  isc_spb_bkp_file                 = 5;
  isc_spb_bkp_factor               = 6;
  isc_spb_bkp_length               = 7;
  isc_spb_bkp_ignore_checksums     = $01;
  isc_spb_bkp_ignore_limbo         = $02;
  isc_spb_bkp_metadata_only        = $04;
  isc_spb_bkp_no_garbage_collect   = $08;
  isc_spb_bkp_old_descriptions     = $10;
  isc_spb_bkp_non_transportable    = $20;
  isc_spb_bkp_convert              = $40;
  isc_spb_bkp_expand		 = $80;

  {*******************************************
   * Parameters for isc_action_svc_properties *
   *******************************************}

  isc_spb_prp_page_buffers		= 5;
  isc_spb_prp_sweep_interval		= 6;
  isc_spb_prp_shutdown_db			= 7;
  isc_spb_prp_deny_new_attachments	= 9;
  isc_spb_prp_deny_new_transactions	= 10;
  isc_spb_prp_reserve_space		= 11;
  isc_spb_prp_write_mode			= 12;
  isc_spb_prp_access_mode			= 13;
  isc_spb_prp_set_sql_dialect		= 14;
  isc_spb_prp_activate			= $0100;
  isc_spb_prp_db_online			= $0200;

  {*******************************************
   * Parameters for isc_spb_prp_reserve_space *
   *******************************************}

  isc_spb_prp_res_use_full	= 35;
  isc_spb_prp_res			= 36;

  {*****************************************
   * Parameters for isc_spb_prp_write_mode  *
   *****************************************}

  isc_spb_prp_wm_async		= 37;
  isc_spb_prp_wm_sync		= 38;

  {*****************************************
   * Parameters for isc_spb_prp_access_mode *
   *****************************************}

  isc_spb_prp_am_readonly		= 39;
  isc_spb_prp_am_readwrite	= 40;

  {****************************************
   * Parameters for isc_action_svc_repair  *
   ****************************************}

  isc_spb_rpr_commit_trans		= 15;
  isc_spb_rpr_rollback_trans		= 34;
  isc_spb_rpr_recover_two_phase		= 17;
  isc_spb_tra_id                     	= 18;
  isc_spb_single_tra_id			= 19;
  isc_spb_multi_tra_id			= 20;
  isc_spb_tra_state			= 21;
  isc_spb_tra_state_limbo			= 22;
  isc_spb_tra_state_commit		= 23;
  isc_spb_tra_state_rollback		= 24;
  isc_spb_tra_state_unknown		= 25;
  isc_spb_tra_host_site			= 26;
  isc_spb_tra_remote_site			= 27;
  isc_spb_tra_db_path			= 28;
  isc_spb_tra_advise			= 29;
  isc_spb_tra_advise_commit		= 30;
  isc_spb_tra_advise_rollback		= 31;
  isc_spb_tra_advise_unknown		= 33;

  isc_spb_rpr_validate_db			= $01;
  isc_spb_rpr_sweep_db			= $02;
  isc_spb_rpr_mend_db			= $04;
  isc_spb_rpr_list_limbo_trans		= $08;
  isc_spb_rpr_check_db			= $10;
  isc_spb_rpr_ignore_checksum		= $20;
  isc_spb_rpr_kill_shadows		= $40;
  isc_spb_rpr_full			= $80;

  {****************************************
   * Parameters for isc_action_svc_restore *
   ****************************************}

  isc_spb_res_buffers			= 9;
  isc_spb_res_page_size			= 10 ;
  isc_spb_res_length			= 11;
  isc_spb_res_access_mode			= 12;
  isc_spb_res_deactivate_idx		= $0100;
  isc_spb_res_no_shadow			= $0200;
  isc_spb_res_no_validity			= $0400;
  isc_spb_res_one_at_a_time		= $0800;
  isc_spb_res_replace			= $1000;
  isc_spb_res_create			= $2000;
  isc_spb_res_use_all_space		= $4000;
  isc_spb_res_validate			= $8000;

  {*****************************************
   * Parameters for isc_spb_res_access_mode  *
   *****************************************}

  isc_spb_res_am_readonly			= isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite		= isc_spb_prp_am_readwrite;

  {******************************************
   * Parameters for isc_info_svc_svr_db_info *
   ******************************************}

  isc_spb_num_att               = 5;
  isc_spb_num_db                = 6;

  {****************************************
   * Parameters for isc_info_svc_db_stats  *
   ****************************************}

  isc_spb_sts_data_pages		= $01;
  isc_spb_sts_db_log		= $02;
  isc_spb_sts_hdr_pages		= $04;
  isc_spb_sts_idx_pages		= $08;
  isc_spb_sts_sys_relations	= $10;
  isc_spb_sts_record_versions	= $20;
  isc_spb_sts_table		= $40;

  {***********************}
  { SQL information items }
  {***********************}

  isc_info_sql_select               = 4;
  isc_info_sql_bind                 = 5;
  isc_info_sql_num_variables        = 6;
  isc_info_sql_describe_vars        = 7;
  isc_info_sql_describe_end         = 8;
  isc_info_sql_sqlda_seq            = 9;
  isc_info_sql_message_seq          = 10;
  isc_info_sql_type                 = 11;
  isc_info_sql_sub_type             = 12;
  isc_info_sql_scale                = 13;
  isc_info_sql_length               = 14;
  isc_info_sql_null_ind             = 15;
  isc_info_sql_field                = 16;
  isc_info_sql_relation             = 17;
  isc_info_sql_owner                = 18;
  isc_info_sql_alias                = 19;
  isc_info_sql_sqlda_start          = 20;
  isc_info_sql_stmt_type            = 21;
  isc_info_sql_get_plan             = 22;
  isc_info_sql_records		  = 23;
  isc_info_sql_batch_fetch	  = 24;
  isc_info_sql_precision            = 25;

  {*******************************}
  { SQL information return values }
  {*******************************}

  isc_info_sql_stmt_select          = 1;
  isc_info_sql_stmt_insert          = 2;
  isc_info_sql_stmt_update          = 3;
  isc_info_sql_stmt_delete          = 4;
  isc_info_sql_stmt_ddl             = 5;
  isc_info_sql_stmt_get_segment     = 6;
  isc_info_sql_stmt_put_segment     = 7;
  isc_info_sql_stmt_exec_procedure  = 8;
  isc_info_sql_stmt_start_trans     = 9;
  isc_info_sql_stmt_commit          = 10;
  isc_info_sql_stmt_rollback        = 11;
  isc_info_sql_stmt_select_for_upd  = 12;
  isc_info_sql_stmt_set_generator   = 13;

  {*********************************}
  { Server configuration key values }
  {*********************************}

const
  ISCCFG_LOCKMEM_KEY	= 0;
  ISCCFG_LOCKSEM_KEY	= 1;
  ISCCFG_LOCKSIG_KEY	= 2;
  ISCCFG_EVNTMEM_KEY	= 3;
  ISCCFG_DBCACHE_KEY	= 4;
  ISCCFG_PRIORITY_KEY	= 5;
  ISCCFG_IPCMAP_KEY	= 6;
  ISCCFG_MEMMIN_KEY	= 7;
  ISCCFG_MEMMAX_KEY	= 8;
  ISCCFG_LOCKORDER_KEY	= 9;
  ISCCFG_ANYLOCKMEM_KEY	= 10;
  ISCCFG_ANYLOCKSEM_KEY	= 11;
  ISCCFG_ANYLOCKSIG_KEY	= 12;
  ISCCFG_ANYEVNTMEM_KEY	= 13;
  ISCCFG_LOCKHASH_KEY	= 14;
  ISCCFG_DEADLOCK_KEY	= 15;
  ISCCFG_LOCKSPIN_KEY	= 16;
  ISCCFG_CONN_TIMEOUT_KEY = 17;
  ISCCFG_DUMMY_INTRVL_KEY = 18;
  ISCCFG_TRACE_POOLS_KEY  = 19   { Internal Use only };
  ISCCFG_REMOTE_BUFFER_KEY	= 20;
  ISCCFG_CPU_AFFINITY_KEY	= 21;
  ISCCFG_SWEEP_QUANTUM_KEY	= 22;
  ISCCFG_USER_QUANTUM_KEY	    = 23;
  ISCCFG_SLEEP_TIME_KEY	= 24;
  ISCCFG_MAX_THREADS_KEY	= 25;
  ISCCFG_ADMIN_DB_KEY	= 26;
  ISCCFG_USE_SANCTUARY_KEY	= 27;
  ISCCFG_ENABLE_HT_KEY	= 28;
  ISCCFG_USE_ROUTER_KEY	= 29;
  ISCCFG_SORTMEM_BUFFER_SIZE_KEY	= 30;
  ISCCFG_SQL_CMP_RECURSION_KEY	= 31;
  ISCCFG_SOL_BOUND_THREADS_KEY	= 32;
  ISCCFG_SOL_SYNC_SCOPE_KEY	= 33;

  {*************}
  { Error codes }
  {*************}

  isc_facility                       = 20;
  { END CONVERT TAG }
  { START CONVERT TAG Define as long }
  isc_err_base                       = 335544320;
  { END CONVERT TAG }
  { START CONVERT TAG }
  isc_err_factor                     = 1;
  isc_arg_end                        = 0;
  isc_arg_gds                        = 1;
  isc_arg_string                     = 2;
  isc_arg_cstring                    = 3;
  isc_arg_number                     = 4;
  isc_arg_interpreted                = 5;
  isc_arg_vms                        = 6;
  isc_arg_unix                       = 7;
  isc_arg_domain                     = 8;
  isc_arg_dos                        = 9;
  isc_arg_mpexl                      = 10;
  isc_arg_mpexl_ipc                  = 11;
  isc_arg_next_mach		   = 15;
  isc_arg_netware		           = 16;
  isc_arg_win32                      = 17;
  isc_arg_warning                    = 18;
  isc_arg_sql                        = 19;

  {********************************************}
  { Dynamic Data Definition Language operators }
  {********************************************}

  {****************}
  { Version number }
  {****************}

  isc_dyn_version_1                 = 1;
  isc_dyn_eoc                       = 255;

  {****************************}
  { Operations (may be nested) }
  {****************************}

  isc_dyn_begin                     = 2;
  isc_dyn_end                       = 3;
  isc_dyn_if                        = 4;
  isc_dyn_def_database              = 5;
  isc_dyn_def_global_fld            = 6;
  isc_dyn_def_local_fld             = 7;
  isc_dyn_def_idx                   = 8;
  isc_dyn_def_rel                   = 9;
  isc_dyn_def_sql_fld               = 10;
  isc_dyn_def_view                  = 12;
  isc_dyn_def_trigger               = 15;
  isc_dyn_def_security_class        = 120;
  isc_dyn_def_dimension             = 140;
  isc_dyn_def_generator             = 24;
  isc_dyn_def_function              = 25;
  isc_dyn_def_filter                = 26;
  isc_dyn_def_function_arg          = 27;
  isc_dyn_def_shadow                = 34;
  isc_dyn_def_trigger_msg           = 17;
  isc_dyn_def_file                  = 36;
  isc_dyn_def_user                  = 225;
  isc_dyn_def_journal               = 203;
  isc_dyn_def_archive               = 206;
  isc_dyn_mod_database              = 39;
  isc_dyn_mod_rel                   = 11;
  isc_dyn_mod_global_fld            = 13;
  isc_dyn_mod_idx                   = 102;
  isc_dyn_mod_local_fld             = 14;
  isc_dyn_mod_sql_fld		  = 216;
  isc_dyn_mod_view                  = 16;
  isc_dyn_mod_security_class        = 122;
  isc_dyn_mod_trigger               = 113;
  isc_dyn_mod_trigger_msg           = 28;
  isc_dyn_mod_user                  = 226;
  isc_dyn_mod_journal               = 204;
  isc_dyn_mod_archive               = 207;
  isc_dyn_delete_database           = 18;
  isc_dyn_delete_rel                = 19;
  isc_dyn_delete_global_fld         = 20;
  isc_dyn_delete_local_fld          = 21;
  isc_dyn_delete_idx                = 22;
  isc_dyn_delete_security_class     = 123;
  isc_dyn_delete_dimensions         = 143;
  isc_dyn_delete_trigger            = 23;
  isc_dyn_delete_trigger_msg        = 29;
  isc_dyn_delete_filter             = 32;
  isc_dyn_delete_function           = 33;
  isc_dyn_delete_generator          = 217;
  isc_dyn_delete_shadow             = 35;
  isc_dyn_delete_user               = 227;
  isc_dyn_delete_journal            = 205;
  isc_dyn_delete_archive            = 208;
  isc_dyn_grant                     = 30;
  isc_dyn_revoke                    = 31;
  isc_dyn_def_primary_key           = 37;
  isc_dyn_def_foreign_key           = 38;
  isc_dyn_def_unique                = 40;
  isc_dyn_def_procedure             = 164;
  isc_dyn_delete_procedure          = 165;
  isc_dyn_def_parameter             = 135;
  isc_dyn_delete_parameter          = 136;
  isc_dyn_mod_procedure             = 175;
  isc_dyn_def_log_file              = 176;
  isc_dyn_def_exception             = 181;
  isc_dyn_mod_exception             = 182;
  isc_dyn_del_exception             = 183;
  isc_dyn_def_default_log           = 202;

  {*********************}
  { View specific stuff }
  {*********************}

  isc_dyn_view_blr                  = 43;
  isc_dyn_view_source               = 44;
  isc_dyn_view_relation             = 45;
  isc_dyn_view_context              = 46;
  isc_dyn_view_context_name         = 47;

  {********************}
  { Generic attributes }
  {********************}

  isc_dyn_rel_name                  = 50;
  isc_dyn_fld_name                  = 51;
  isc_dyn_new_fld_name		  = 215;
  isc_dyn_idx_name                  = 52;
  isc_dyn_description               = 53;
  isc_dyn_security_class            = 54;
  isc_dyn_system_flag               = 55;
  isc_dyn_update_flag               = 56;
  isc_dyn_prc_name                  = 166;
  isc_dyn_prm_name                  = 137;
  isc_dyn_sql_object                = 196;
  isc_dyn_fld_character_set_name    = 174;
  isc_dyn_restrict_or_cascade       = 220;

  {******************************}
  { Relation specific attributes }
  {******************************}

  isc_dyn_rel_dbkey_length          = 61;
  isc_dyn_rel_store_trig            = 62;
  isc_dyn_rel_modify_trig           = 63;
  isc_dyn_rel_erase_trig            = 64;
  isc_dyn_rel_store_trig_source     = 65;
  isc_dyn_rel_modify_trig_source    = 66;
  isc_dyn_rel_erase_trig_source     = 67;
  isc_dyn_rel_ext_file              = 68;
  isc_dyn_rel_sql_protection        = 69;
  isc_dyn_rel_constraint            = 162;
  isc_dyn_delete_rel_constraint     = 163;
  isc_dyn_rel_sql_scope             = 218;
  isc_dyn_rel_sql_on_commit         = 219;

  {**********************************}
  { Global field specific attributes }
  {**********************************}

  isc_dyn_fld_type                  = 70;
  isc_dyn_fld_length                = 71;
  isc_dyn_fld_scale                 = 72;
  isc_dyn_fld_sub_type              = 73;
  isc_dyn_fld_segment_length        = 74;
  isc_dyn_fld_query_header          = 75;
  isc_dyn_fld_edit_string           = 76;
  isc_dyn_fld_validation_blr        = 77;
  isc_dyn_fld_validation_source     = 78;
  isc_dyn_fld_computed_blr          = 79;
  isc_dyn_fld_computed_source       = 80;
  isc_dyn_fld_missing_value         = 81;
  isc_dyn_fld_default_value         = 82;
  isc_dyn_fld_query_name            = 83;
  isc_dyn_fld_dimensions            = 84;
  isc_dyn_fld_not_null              = 85;
  isc_dyn_fld_precision             = 86;
  isc_dyn_fld_char_length           = 172;
  isc_dyn_fld_collation             = 173;
  isc_dyn_fld_default_source        = 193;
  isc_dyn_del_default               = 197;
  isc_dyn_del_validation            = 198;
  isc_dyn_single_validation         = 199;
  isc_dyn_fld_character_set         = 203;

  {*********************************}
  { Local field specific attributes }
  {*********************************}

  isc_dyn_fld_source                = 90;
  isc_dyn_fld_base_fld              = 91;
  isc_dyn_fld_position              = 92;
  isc_dyn_fld_update_flag           = 93;

  {***************************}
  { Index specific attributes }
  {***************************}

  isc_dyn_idx_unique                = 100;
  isc_dyn_idx_inactive              = 101;
  isc_dyn_idx_type                  = 103;
  isc_dyn_idx_foreign_key           = 104;
  isc_dyn_idx_ref_column            = 105;
  isc_dyn_idx_statistic		  = 204;

  {*****************************}
  { Trigger specific attributes }
  {*****************************}

  isc_dyn_trg_type                  = 110;
  isc_dyn_trg_blr                   = 111;
  isc_dyn_trg_source                = 112;
  isc_dyn_trg_name                  = 114;
  isc_dyn_trg_sequence              = 115;
  isc_dyn_trg_inactive              = 116;
  isc_dyn_trg_msg_number            = 117;
  isc_dyn_trg_msg                   = 118;

  {************************************}
  { Security Class specific attributes }
  {************************************}

  isc_dyn_scl_acl                   = 121;
  isc_dyn_grant_user                = 130;
  isc_dyn_grant_proc                = 186;
  isc_dyn_grant_trig                = 187;
  isc_dyn_grant_view                = 188;
  isc_dyn_grant_options             = 132;
  isc_dyn_grant_user_group          = 205;


  {********************************}
  { Dimension specific information }
  {********************************}

  isc_dyn_dim_lower                 = 141;
  isc_dyn_dim_upper                 = 142;

  {**************************}
  { File specific attributes }
  {**************************}

  isc_dyn_file_name                 = 125;
  isc_dyn_file_start                = 126;
  isc_dyn_file_length               = 127;
  isc_dyn_shadow_number             = 128;
  isc_dyn_shadow_man_auto           = 129;
  isc_dyn_shadow_conditional        = 130;

  {******************************}
  { Log file specific attributes }
  {******************************}

  isc_dyn_log_file_sequence         = 177;
  isc_dyn_log_file_partitions       = 178;
  isc_dyn_log_file_serial           = 179;
  isc_dyn_log_file_directory        = 200;
  isc_dyn_log_file_raw		  = 201;

  {*************************}
  { Log specific attributes }
  {*************************}

  isc_dyn_log_check_point_interval  = 189;
  isc_dyn_log_buffer_size           = 190;
  isc_dyn_log_check_point_length    = 191;
  isc_dyn_log_num_of_buffers        = 192;
  isc_dyn_log_timestamp_name        = 193;

  {******************************}
  { Function specific attributes }
  {******************************}

  isc_dyn_function_name             = 145;
  isc_dyn_function_type             = 146;
  isc_dyn_func_module_name          = 147;
  isc_dyn_func_entry_point          = 148;
  isc_dyn_func_return_argument      = 149;
  isc_dyn_func_arg_position         = 150;
  isc_dyn_func_mechanism            = 151;
  isc_dyn_filter_in_subtype         = 152;
  isc_dyn_filter_out_subtype        = 153;


  isc_dyn_description2		  = 154;
  isc_dyn_fld_computed_source2	  = 155;
  isc_dyn_fld_edit_string2	  = 156;
  isc_dyn_fld_query_header2	  = 157;
  isc_dyn_fld_validation_source2	  = 158;
  isc_dyn_trg_msg2		  = 159;
  isc_dyn_trg_source2		  = 160;
  isc_dyn_view_source2		  = 161;
  isc_dyn_xcp_msg2		  = 184;

  {*******************************}
  { Generator specific attributes }
  {*******************************}

  isc_dyn_generator_name            = 95;
  isc_dyn_generator_id              = 96;

  {*******************************}
  { Procedure specific attributes }
  {*******************************}

  isc_dyn_prc_inputs                = 167;
  isc_dyn_prc_outputs               = 168;
  isc_dyn_prc_source                = 169;
  isc_dyn_prc_blr                   = 170;
  isc_dyn_prc_source2               = 171;

  {*******************************}
  { Parameter specific attributes }
  {*******************************}

  isc_dyn_prm_number                = 138;
  isc_dyn_prm_type                  = 139;

  {******************************}
  { Relation specific attributes }
  {******************************}

  isc_dyn_xcp_msg                   = 185;

  {********************************************}
  { Cascading referential integrity values     }
  {********************************************}
  isc_dyn_foreign_key_update        = 205;
  isc_dyn_foreign_key_delete        = 206;
  isc_dyn_foreign_key_cascade       = 207;
  isc_dyn_foreign_key_default       = 208;
  isc_dyn_foreign_key_null          = 209;
  isc_dyn_foreign_key_none          = 210;

  {*********************}
  { SQL role values     }
  {*********************}
  isc_dyn_def_sql_role              = 211;
  isc_dyn_sql_role_name             = 212;
  isc_dyn_grant_admin_options       = 213;
  isc_dyn_del_sql_role              = 214;

  {*********************}
  { ADMIN OPTION values }
  {*********************}
  isc_dyn_add_admin                 = 221;
  isc_dyn_drop_admin                = 222;
  isc_dyn_admin_active              = 223;
  isc_dyn_admin_inactive            = 224;

  {**************************}
  { User specific attributes }
  {**************************}
  isc_dyn_user_sys_name             = 11;
  isc_dyn_user_grp_name             = 12;
  isc_dyn_user_uid                  = 13;
  isc_dyn_user_gid                  = 14;
  isc_dyn_user_password             = 15;
  isc_dyn_user_active               = 16;
  isc_dyn_user_inactive             = 17;
  isc_dyn_user_description          = 18;
  isc_dyn_user_first_name           = 19;
  isc_dyn_user_middle_name          = 20;
  isc_dyn_user_last_name            = 21;
  isc_dyn_user_default_role         = 22;

  {**************************}
  { Database specific attributes }
  {**************************}
  isc_dyn_db_page_cache             = 41;
  isc_dyn_db_proc_cache             = 42;
  isc_dyn_db_rel_cache              = 43;
  isc_dyn_db_trig_cache             = 44;
  isc_dyn_db_flush_int              = 45;
  isc_dyn_db_linger_int             = 46;
  isc_dyn_db_reclaim_int            = 47;
  isc_dyn_db_sweep_int              = 48;
  isc_dyn_db_group_commit           = 49;
  {**************************}
  { Last $dyn value assigned }
  {**************************}

  isc_dyn_last_dyn_value            = 227;

  {****************************************}
  { Array slice description language (SDL) }
  {****************************************}

  isc_sdl_version1                  = 1;
  isc_sdl_eoc                       = 255;
  isc_sdl_relation                  = 2;
  isc_sdl_rid                       = 3;
  isc_sdl_field                     = 4;
  isc_sdl_fid                       = 5;
  isc_sdl_struct                    = 6;
  isc_sdl_variable                  = 7;
  isc_sdl_scalar                    = 8;
  isc_sdl_tiny_integer              = 9;
  isc_sdl_short_integer             = 10;
  isc_sdl_long_integer              = 11;
  isc_sdl_literal                   = 12;
  isc_sdl_add                       = 13;
  isc_sdl_subtract                  = 14;
  isc_sdl_multiply                  = 15;
  isc_sdl_divide                    = 16;
  isc_sdl_negate                    = 17;
  isc_sdl_eql                       = 18;
  isc_sdl_neq                       = 19;
  isc_sdl_gtr                       = 20;
  isc_sdl_geq                       = 21;
  isc_sdl_lss                       = 22;
  isc_sdl_leq                       = 23;
  isc_sdl_and                       = 24;
  isc_sdl_or                        = 25;
  isc_sdl_not                       = 26;
  isc_sdl_while                     = 27;
  isc_sdl_assignment                = 28;
  isc_sdl_label                     = 29;
  isc_sdl_leave                     = 30;
  isc_sdl_begin                     = 31;
  isc_sdl_end                       = 32;
  isc_sdl_do3                       = 33;
  isc_sdl_do2                       = 34;
  isc_sdl_do1                       = 35;
  isc_sdl_element                   = 36;

  {******************************************}
  { International text interpretation values }
  {******************************************}

  isc_interp_eng_ascii              = 0;
  isc_interp_jpn_sjis               = 5;
  isc_interp_jpn_euc                = 6;

  {*****************}
  { SQL definitions }
  {*****************}

  SQL_TEXT                           = 452;
  SQL_VARYING                        = 448;
  SQL_SHORT                          = 500;
  SQL_LONG                           = 496;
  SQL_FLOAT                          = 482;
  SQL_DOUBLE                         = 480;
  SQL_D_FLOAT                        = 530;
  SQL_TIMESTAMP                      = 510;
  SQL_BLOB                           = 520;
  SQL_ARRAY                          = 540;
  SQL_QUAD                           = 550;
  SQL_TYPE_TIME			   = 560;
  SQL_TYPE_DATE                      = 570;
  SQL_INT64			   = 580;
  SQL_BOOLEAN   			   = 590;

  { Historical alias for pre V6 applications }
  SQL_DATE			= SQL_TIMESTAMP;

  {***************}
  { Blob Subtypes }
  {***************}

  { types less than zero are reserved for customer use }

  isc_blob_untyped                   = 0;

  { internal subtypes }

  isc_blob_text                      = 1;
  isc_blob_blr                       = 2;
  isc_blob_acl                       = 3;
  isc_blob_ranges                    = 4;
  isc_blob_summary                   = 5;
  isc_blob_format                    = 6;
  isc_blob_tra                       = 7;
  isc_blob_extfile                   = 8;

  { the range 20-30 is reserved for dBASE and Paradox types }

  isc_blob_formatted_memo            = 20;
  isc_blob_paradox_ole               = 21;
  isc_blob_graphic                   = 22;
  isc_blob_dbase_ole                 = 23;
  isc_blob_typed_binary              = 24;


  { BLOB UDF related things }

type
  TBlob_Get_Segment_Function = function(blob_handle : isc_blob_handle;
                                        buffer : Pointer;
                                        buffer_size : ISC_USHORT;
                                        var result_length : ISC_USHORT): Short cdecl;

  TBlob_Put_Segment_Function = procedure(blob_handle : isc_blob_handle;
                                         buffer : Pointer;
                                         buff_size : ISC_USHORT); cdecl;

  Blob = packed record
    blob_get_segment : TBlob_Get_Segment_Function;
    blob_handle : isc_blob_handle;
    number_segments : long;
    max_seglen : long;
    total_size : long;
    blob_put_segment : TBlob_Put_Segment_Function;
  end;                                                                                             
  PBlob = ^Blob;

  { ib_util }

  function ib_util_malloc(l: integer): pointer; cdecl; external ib_utildll;


function IB2007DecodeTimestamp(ATimestamp : ISC_TIMESTAMP) : TDateTime;

implementation

function IB2007DecodeTimestamp(ATimestamp : ISC_TIMESTAMP) : TDateTime;
var
  Century, Y, M, D,
  H, Mi, S: integer;
  TotalSeconds : Cardinal;
const
  SECONDS_PER_MINUTE = 60;
  MINUTES_PER_HOUR = 60;
  SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;
begin
  ATimestamp.timestamp_date := ATimestamp.timestamp_date - (1721119 - 2400001);
  Century := (4 * ATimestamp.timestamp_date - 1) div 146097;
  ATimestamp.timestamp_date := 4 * ATimestamp.timestamp_date - 1 - 146097 * Century;
  D := ATimestamp.timestamp_date div 4;
  ATimestamp.timestamp_date := (4 * D + 3) div 1461;
  D := 4 * D + 3 - 1461 * ATimestamp.timestamp_date;
  D := (D + 4) div 4;
  M := (5 * D - 3) div 153;
  D := 5 * D - 3 - 153 * M;
  D := (D + 5) div 5;
  Y := 100 * Century + ATimestamp.timestamp_date;

  if M < 10 then
    M := M + 3
  else begin
    M := M - 9;
    Y := Y + 1;
  end;

  TotalSeconds := ATimestamp.timestamp_time div ISC_TIME_SECONDS_PRECISION;

  H := TotalSeconds div SECONDS_PER_HOUR;
  Mi := (TotalSeconds div SECONDS_PER_MINUTE) mod SECONDS_PER_MINUTE;
  S := TotalSeconds mod SECONDS_PER_MINUTE;

  Result := EncodeDateTime(Y, M, D, H, Mi, S, 0);
end;

function getb(var p: BSTREAM): AnsiChar;
begin
  Dec(p.bstr_cnt);
  if (p.bstr_cnt >= 0) then begin
    result := AnsiChar(Int(p.bstr_ptr^) and Int(0377));
    Inc(p.bstr_ptr);
  end else
    result := AnsiChar(BLOB_get(p));
end;

function putb(x: AnsiChar; var p: BSTREAM): Int;
begin
  Dec(p.bstr_cnt);
  if (x = Chr(Int('n') - Int('a'))) or (p.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p.bstr_ptr^ := AnsiChar(x);
    result := Int(x);
    Inc(p.bstr_ptr^);
  end;
end;

function putbx(x: AnsiChar; var p: BSTREAM): Int;
begin
  Dec(p.bstr_cnt);
  if (x = Chr(Int('n') - Int('a'))) or (p.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p.bstr_ptr^ := AnsiChar(x);
    result := Int(x);
    Inc(p.bstr_ptr^);
  end;
end;

function XSQLDA_LENGTH(n : Long) : Long;
begin
  Result := sizeof (XSQLDA) + n * sizeof (XSQLVAR);
end;

function XSQLVAR_LENGTH(num_rows, num_vars : Long) : Long;
begin
  Result := sizeof(XSQLVAR) * num_rows * num_vars;
end;


end.
