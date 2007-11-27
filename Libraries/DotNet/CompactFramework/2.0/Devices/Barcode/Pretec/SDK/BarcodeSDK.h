/*
//=============================================================================
//
//  PRETEC Scan Module SDK Include File
//
//  PRETEC, Inc.  2004
//  http://www.pretec.com/
//
//  (C) Copyright PRETEC Inc. 2004
//  Title:  BarcodeSDK.h
//  Version:    1.20
//  Date:   31 May 2004
//  Author: Terry Wu
//
//=============================================================================
*/

typedef BOOL	(*WMS_Open)(CString,DWORD);
typedef void	(*WMS_Close)();
typedef BOOL	(*WMS_Scan)(BYTE*,int*);
typedef BOOL	(*WMS_SetConfig)(unsigned long,BYTE);

// BARCODE_TYPE
#define  BARCODE_TYPE_UPC_A		0x0001
#define  BARCODE_TYPE_EAN_13		0x0002	
#define  BARCODE_TYPE_EAN_8		0x0004
#define  BARCODE_TYPE_UPC_E		0x0008
#define  BARCODE_TYPE_CODE_39		0x0010
#define  BARCODE_TYPE_CODE_32		0x0020
#define  BARCODE_TYPE_CODE_128		0x0040
#define  BARCODE_TYPE_INTERLEAVE_25	0x0080
#define  BARCODE_TYPE_INDUSTRIAL_25	0x0100
#define  BARCODE_TYPE_MATRIX_25		0x0200
#define  BARCODE_TYPE_CODABAR		0x0400
#define  BARCODE_TYPE_CODE_93		0x0800	
#define  BARCODE_TYPE_CODE_11		0x1000
#define  BARCODE_TYPE_CHINA_POSTAGE	0x2000
#define  BARCODE_TYPE_MSI		0x4000
#define  BARCODE_TYPE_BC412		0x8000
#define  BARCODE_TYPE_CODE_2_OF_6	0x00010000
#define  BARCODE_TYPE_RESERVE_4_CODE	0x00020000
#define  BARCODE_TYPE_VP_25		0x00040000
#define  BARCODE_TYPE_TELEPEN		0x00080000
#define  BARCODE_TYPE_ALL		0x000FFFFF

//BARCODE_CASE
#define  BARCODE_CASE_NOT_CHANGED	0x00
#define  BARCODE_CASE_UPPER_CASE	0x01
#define  BARCODE_CASE_LOWER_CASE	0x02

/////////////////////////////////////
// End of file "pretec.h"
/////////////////////////////////////
