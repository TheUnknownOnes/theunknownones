

#include "stdafx.h"
#include "cdType.h"
#include "cderror.h"

#include "MyStream.h"

#define		MODE_MEMSTREAM_OPEN		0x01
#define		MODE_MEMSTREAM_READ		0x02
#define		MODE_MEMSTREAM_WRITE	0x04


/* The internal data structure object of a stream */
typedef	struct	tagMemStreamData
{
	cdChar						mode;
	cdInt32						lPos;
	cdUInt32					dwVisibleSize;
	cdUInt32					dwBufferSize;
	cdChar						*cpBuffer;
}MemStreamData;

typedef	struct	tagFilStreamData
{
	cdChar						szFileName[MAX_PATH];
	HANDLE						hFile;
}FilStreamData;


/* Prototype declaration */
/* memory stream */
void cdSTDCALL _OpenMyMemStream( cdContext, cdPermission, cdError* );
void cdSTDCALL _CloseMyMemStream( cdContext, cdError* );
void cdSTDCALL _ReadMyMemStream( cdContext, void*, cdUInt32*, cdError* );
void cdSTDCALL _WriteMyMemStream( cdContext, const void*, cdUInt32*, cdError* );
void cdSTDCALL _SeekMyMemStream( cdContext, cdWhence, cdInt32, cdError* );
cdInt32 cdSTDCALL _TellMyMemStream( cdContext, cdError* );
/* file stream */
void cdSTDCALL _OpenMyFilStream( cdContext, cdPermission, cdError* );
void cdSTDCALL _CloseMyFilStream( cdContext, cdError* );
void cdSTDCALL _ReadMyFilStream( cdContext, void*, cdUInt32*, cdError* );
void cdSTDCALL _WriteMyFilStream( cdContext, const void*, cdUInt32*, cdError* );
void cdSTDCALL _SeekMyFilStream( cdContext, cdWhence, cdInt32, cdError* );
cdInt32 cdSTDCALL _TellMyFilStream( cdContext, cdError* );



///////////////////////////////////////////////////////////////
//	My memory stream

/* A stream is created. */
BOOL	CreateMyMemStream(	cdStream	*pStream,
							cdUInt32	dwSize )
{
	MemStreamData	*pMemStrm;
	
	/* The domain for data is secured. */
	pMemStrm = new MemStreamData;
	if( pMemStrm == NULL )
	{
		return	FALSE;
	}
	
	/* A buffer is secured in the appointed size. */
	if( dwSize )
	{
		pMemStrm->cpBuffer = new cdChar[dwSize];
		if( pMemStrm->cpBuffer == NULL )
		{
			return	FALSE;
		}
	}
	else
	{
		pMemStrm->cpBuffer = NULL;
	}
	
	/* Data is changed the first stage. */
	pMemStrm->mode = 0;
	pMemStrm->lPos = 0;
	pMemStrm->dwVisibleSize = 0;
	pMemStrm->dwBufferSize = dwSize;
	
	pStream->contextH = (cdContext)pMemStrm;
	pStream->close = _CloseMyMemStream;
	pStream->open = _OpenMyMemStream;
	pStream->read = _ReadMyMemStream;
	pStream->seek = _SeekMyMemStream;
	pStream->tell = _TellMyMemStream;
	pStream->write = _WriteMyMemStream;
	
	return	TRUE;
}

/* A stream is released. */
void	ReleaseMyMemStream( cdStream	*pStream )
{
	MemStreamData	*pMemStrm;
	
	/* A domain is released. */
	pMemStrm = (MemStreamData*)pStream->contextH;
	if( pMemStrm )
	{
		if( pMemStrm->cpBuffer )
		{
			delete	[]pMemStrm->cpBuffer;
			pMemStrm->cpBuffer = NULL;
		}
		
		delete	pMemStrm;
		pStream->contextH = (cdContext)NULL;
	}
	
	pStream->close = NULL;
	pStream->open = NULL;
	pStream->read = NULL;
	pStream->seek = NULL;
	pStream->tell = NULL;
	pStream->write = NULL;
}

/* The buffer size where the data of a stream is saved is acquired. */
cdUInt32	GetMyMemStreamSize( cdStream	*pStream )
{
	MemStreamData	*pMemStrm;
	
	
	pMemStrm = (MemStreamData*)pStream->contextH;
	if( !pMemStrm )
	{
		return	0;
	}
	
	return		pMemStrm->dwVisibleSize;
}

/* A stream is copied. */
void	CopyMyMemStream(	cdStream	*pSrcStram,
							cdStream	*pDstStram,
							cdUInt32	*pCpySize,
							cdError		*pErr )
{
	cdUInt32		Size;
	MemStreamData	*pDstStrm;
	cdError			err=cdOK;
	cdChar			*cpNewBuffer;
	
	
	Size = *pCpySize;
	err = cdOK;
	pDstStrm = (MemStreamData*)pDstStram->contextH;
	if( pDstStrm->dwBufferSize < Size )
	{
		cpNewBuffer = new cdChar[Size];
		if( cpNewBuffer )
		{
			memcpy( cpNewBuffer, pDstStrm->cpBuffer, pDstStrm->dwBufferSize );
			memset( pDstStrm->cpBuffer+pDstStrm->dwBufferSize, 0, Size );
			delete	[]pDstStrm->cpBuffer;
			pDstStrm->cpBuffer = cpNewBuffer;
			pDstStrm->dwBufferSize = Size;
		}
		else
		{
			Size = pDstStrm->dwBufferSize;
			err = cdSTREAM_WRITE_ERROR;
		}
	}
	
	pSrcStram->read( pSrcStram->contextH, pDstStrm->cpBuffer, &Size, pErr );
	if( *pErr == cdOK )
	{
		*pErr = err;
	}
	
	if( pDstStrm->dwVisibleSize < Size )
	{
		pDstStrm->dwVisibleSize = Size;
	}
	
	*pCpySize = Size;
}

/* The contents of a stream are saved at a file. */
void	SaveMyMemStream(	cdStream	*pSrcStram,
							cdChar		*szPath,
							cdError		*pErr )
{
	HANDLE			hFile;
	MemStreamData	*pMemStrm;
	DWORD			dwWrite;
	BOOL			fRes;
	
	
	*pErr = cdOK;
	hFile = CreateFile(	szPath,
						GENERIC_WRITE,
						0,
						NULL,
						CREATE_ALWAYS,
						FILE_ATTRIBUTE_NORMAL,
						NULL );
	if( hFile == INVALID_HANDLE_VALUE )
	{
		*pErr = cdFILE_OPEN_ERROR;
		return;
	}
	
	pMemStrm = (MemStreamData*)pSrcStram->contextH;
	fRes = WriteFile( hFile, pMemStrm->cpBuffer, pMemStrm->dwVisibleSize, &dwWrite, NULL );
	if( fRes == FALSE )
	{
		*pErr = cdFILE_WRITE_ERROR;
		CloseHandle( hFile );
		return;
	}
	
	fRes = CloseHandle( hFile );
	if( fRes == FALSE )
	{
		*pErr = cdFILE_CLOSE_ERROR;
		return;
	}
	
}

/* The OPEN function registered into a stream */
void cdSTDCALL _OpenMyMemStream(	cdContext		contextH,
									cdPermission	mode,
									cdError			*pErr )
{
	MemStreamData	*pMemStrm;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_OPEN_ERROR;
		return;
	}
	
	if( pMemStrm->mode&MODE_MEMSTREAM_OPEN )
	{
		*pErr = cdSTREAM_ALREADY_OPEN;
		return;
	}
	
	/* The current position is set up. */
	switch( mode )
	{
		case cdREAD:
			pMemStrm->mode = MODE_MEMSTREAM_OPEN|MODE_MEMSTREAM_READ;
			pMemStrm->lPos = 0;
			break;
		case cdWRITE:
			pMemStrm->mode = MODE_MEMSTREAM_OPEN|MODE_MEMSTREAM_WRITE;
			pMemStrm->lPos = 0;
			break;
		case cdUPDATE:
			pMemStrm->mode = MODE_MEMSTREAM_OPEN|MODE_MEMSTREAM_WRITE|MODE_MEMSTREAM_READ;
			pMemStrm->lPos = 0;
			break;
		default:
			*pErr = cdSTREAM_BAD_OPTIONS;
			return;
	}
	
}

/* The CLOSE function registered into a stream */
void cdSTDCALL _CloseMyMemStream(	cdContext	contextH,
									cdError		*pErr )
{
	MemStreamData	*pMemStrm;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_CLOSE_ERROR;
		return;
	}
	
	pMemStrm->mode = 0;
	
}

/* The READ function registered into a stream */
void cdSTDCALL _ReadMyMemStream(	cdContext	contextH,
									void		*pBuf,
									cdUInt32	*pBufsize,
									cdError		*pErr )
{
	MemStreamData	*pMemStrm;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_READ_ERROR;
		return;
	}
	
	if( !(pMemStrm->mode&MODE_MEMSTREAM_OPEN) )
	{
		*pErr = cdSTREAM_NOT_OPEN;
		return;
	}
	else if( !(pMemStrm->mode&MODE_MEMSTREAM_READ) )
	{
		*pErr = cdSTREAM_PERMISSION_ERROR;
		return;
	}
	
	/* It confirms whether to exceed the size of a buffer. */
	if( pMemStrm->dwVisibleSize < (pMemStrm->lPos+(*pBufsize)) )
	{
		/* It reads as much as possible. */
		memcpy( pBuf, pMemStrm->cpBuffer+pMemStrm->lPos, pMemStrm->dwVisibleSize-pMemStrm->lPos );
		*pBufsize = pMemStrm->dwVisibleSize-pMemStrm->lPos;
		pMemStrm->lPos = pMemStrm->dwVisibleSize;
		*pErr = cdSTREAM_READ_ERROR;
		return;
	}
	
	memcpy( pBuf, pMemStrm->cpBuffer+pMemStrm->lPos, *pBufsize );
	pMemStrm->lPos += (*pBufsize);
	
}

/* The WRITE function registered into a stream */
void cdSTDCALL _WriteMyMemStream(	cdContext	contextH,
									const void	*pBuf,
									cdUInt32	*pBufsize,
									cdError		*pErr )
{
	MemStreamData	*pMemStrm;
	cdChar			*cpNewBuffer;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_WRITE_ERROR;
		return;
	}
	
	if( !(pMemStrm->mode&MODE_MEMSTREAM_OPEN) )
	{
		*pErr = cdSTREAM_NOT_OPEN;
		return;
	}
	else if( !(pMemStrm->mode&MODE_MEMSTREAM_WRITE) )
	{
		*pErr = cdSTREAM_PERMISSION_ERROR;
		return;
	}
	
	/* It investigates whether there is any enough buffer. */
	if( pMemStrm->dwBufferSize < (pMemStrm->lPos+(*pBufsize)) )
	{
		/* A buffer is created newly and data is saved. */
		cpNewBuffer = new cdChar[pMemStrm->lPos+(*pBufsize)];
		if( cpNewBuffer == NULL )
		{
			/* Data is saved as much as possible. */
			memcpy( pMemStrm->cpBuffer+pMemStrm->lPos, pBuf, pMemStrm->dwBufferSize-pMemStrm->lPos );
			*pBufsize = pMemStrm->dwBufferSize-pMemStrm->lPos;
			pMemStrm->lPos = pMemStrm->dwBufferSize;
			*pErr = cdSTREAM_WRITE_ERROR;
			return;
		}
		
		memcpy( cpNewBuffer, pMemStrm->cpBuffer, pMemStrm->lPos );
		memcpy( cpNewBuffer+pMemStrm->lPos, pBuf, *pBufsize );
		
		delete	[]pMemStrm->cpBuffer;
		pMemStrm->cpBuffer = cpNewBuffer;
		pMemStrm->dwBufferSize = pMemStrm->lPos+(*pBufsize);
		pMemStrm->lPos = pMemStrm->lPos+(*pBufsize);
	}
	else
	{
		memcpy( pMemStrm->cpBuffer, pBuf, *pBufsize );
		pMemStrm->lPos += (*pBufsize);
	}
	
	if( pMemStrm->dwVisibleSize < (cdUInt32)pMemStrm->lPos )
	{
		pMemStrm->dwVisibleSize = pMemStrm->lPos;
	}
	
}

/* The SEEK function registered into a stream */
void cdSTDCALL _SeekMyMemStream(	cdContext	contextH,
									cdWhence	origin,
									cdInt32		offset,
									cdError		*pErr )
{
	MemStreamData	*pMemStrm;
	cdInt32			lNewPos;
	cdChar			*cpNewBuffer;
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_SEEK_ERROR;
		return;
	}
	
	if( !(pMemStrm->mode&MODE_MEMSTREAM_OPEN) )
	{
		*pErr = cdSTREAM_NOT_OPEN;
		return;
	}
	
	switch( origin )
	{
		case cdSTART:
			lNewPos = offset;
			break;
		case cdCURRENT:
			lNewPos = pMemStrm->lPos + offset;
			break;
		case cdEND:
			lNewPos = pMemStrm->dwBufferSize - offset;
			break;
		default:
			*pErr = cdSTREAM_BAD_OPTIONS;
			return;
	}
	
	if( (cdUInt32)lNewPos <= pMemStrm->dwBufferSize )
	{
		pMemStrm->lPos = lNewPos;
	}
	else
	{
		/* A buffer is enlarged when larger than a buffer. */
		cpNewBuffer = new cdChar[lNewPos];
		if( cpNewBuffer == NULL )
		{
			*pErr = cdSTREAM_SEEK_ERROR;
			return;
		}
		
		memcpy( cpNewBuffer, pMemStrm->cpBuffer, pMemStrm->dwBufferSize );
		memset( cpNewBuffer+pMemStrm->dwBufferSize, 0, lNewPos-pMemStrm->lPos );
		
		delete	[]pMemStrm->cpBuffer;
		pMemStrm->cpBuffer = cpNewBuffer;
		pMemStrm->dwBufferSize = lNewPos;
		pMemStrm->lPos = lNewPos;
	}
	
	if( pMemStrm->dwVisibleSize < (cdUInt32)pMemStrm->lPos )
	{
		pMemStrm->dwVisibleSize = pMemStrm->lPos;
	}
	
}

/* The TELL function registered into a stream */
cdInt32 cdSTDCALL _TellMyMemStream(	cdContext	contextH,
									cdError		*pErr )
{
	MemStreamData	*pMemStrm;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pMemStrm = (MemStreamData*)contextH;
	if( !pMemStrm )
	{
		*pErr = cdSTREAM_TELL_ERROR;
		return	0;
	}
	
	if( !(pMemStrm->mode&MODE_MEMSTREAM_OPEN) )
	{
		*pErr = cdSTREAM_NOT_OPEN;
		return	0;
	}
	
	return	pMemStrm->lPos;
}


///////////////////////////////////////////////////////////////
//	My file stream

BOOL	CreateMyFilStream(	cdStream	*pStream,
							cdChar		*szFileName )
{
	FilStreamData	*pFilStrm;
	
	/* The domain for data is secured. */
	pFilStrm = new FilStreamData;
	if( pFilStrm == NULL )
	{
		return	FALSE;
	}
	
	/* Data is changed the first stage. */
	pFilStrm->hFile = INVALID_HANDLE_VALUE;
	strcpy( pFilStrm->szFileName, szFileName );
	
	pStream->contextH = (cdContext)pFilStrm;
	pStream->close = _CloseMyFilStream;
	pStream->open = _OpenMyFilStream;
	pStream->read = _ReadMyFilStream;
	pStream->seek = _SeekMyFilStream;
	pStream->tell = _TellMyFilStream;
	pStream->write = _WriteMyFilStream;
	
	return	TRUE;
}

/* A stream is released. */
void	ReleaseMyFilStream( cdStream	*pStream )
{
	FilStreamData	*pFilStrm;
	
	/* A domain is released. */
	pFilStrm = (FilStreamData*)pStream->contextH;
	if( pFilStrm )
	{
		delete	pFilStrm;
		pStream->contextH = (cdContext)NULL;
	}
	
	pStream->close = NULL;
	pStream->open = NULL;
	pStream->read = NULL;
	pStream->seek = NULL;
	pStream->tell = NULL;
	pStream->write = NULL;
}

/* The OPEN function registered into a stream */
void cdSTDCALL _OpenMyFilStream(	cdContext		contextH,
									cdPermission	mode,
									cdError			*pErr )
{
	FilStreamData		*pFilStrm;
	DWORD				dwDesiredAccess,dwShareMode,dwCreationDisposition;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_OPEN_ERROR;
		return;
	}
	
	if( pFilStrm->hFile != INVALID_HANDLE_VALUE )
	{
		*pErr = cdSTREAM_ALREADY_OPEN;
		return;
	}
	
	/* The current position is set up. */
	if( mode == cdUPDATE )
	{
		dwDesiredAccess = GENERIC_WRITE|GENERIC_READ;
		dwShareMode = 0;
		dwCreationDisposition = OPEN_ALWAYS;
	}
	else if( (mode&cdREAD) || (mode&cdWRITE) )
	{
		switch( mode )
		{
			case (cdREAD|cdWRITE):
				dwDesiredAccess = GENERIC_READ|GENERIC_WRITE;
				dwShareMode = 0;
				dwCreationDisposition = CREATE_ALWAYS;
				break;
			case cdREAD:
				dwDesiredAccess = GENERIC_READ;
				dwShareMode = FILE_SHARE_READ;
				dwCreationDisposition = OPEN_EXISTING;
				break;
			case cdWRITE:
				dwDesiredAccess = GENERIC_WRITE;
				dwShareMode = 0;
				dwCreationDisposition = CREATE_ALWAYS;
				break;
		}
	}
	else
	{
		*pErr = cdSTREAM_BAD_OPTIONS;
		return;
	}
	
	pFilStrm->hFile = CreateFile(	pFilStrm->szFileName,
									dwDesiredAccess,
									dwShareMode,
									NULL,
									dwCreationDisposition,
									FILE_ATTRIBUTE_NORMAL,
									NULL );
	if( pFilStrm->hFile == INVALID_HANDLE_VALUE )
	{
		*pErr = cdSTREAM_OPEN_ERROR;
	}
	
}

/* The CLOSE function registered into a stream */
void cdSTDCALL _CloseMyFilStream(	cdContext	contextH,
									cdError		*pErr )
{
	FilStreamData		*pFilStrm;
	BOOL				fRes;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_CLOSE_ERROR;
		return;
	}
	
	fRes = CloseHandle( pFilStrm->hFile );
	if( fRes == FALSE )
	{
		*pErr = cdSTREAM_CLOSE_ERROR;
		return;
	}
	
}

/* The READ function registered into a stream */
void cdSTDCALL _ReadMyFilStream(	cdContext	contextH,
									void		*pBuf,
									cdUInt32	*pBufsize,
									cdError		*pErr )
{
	FilStreamData	*pFilStrm;
	BOOL			fRes;
	DWORD			dwNumberOfBytesToRead;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_READ_ERROR;
		return;
	}
	
	dwNumberOfBytesToRead = *pBufsize;
	fRes = ReadFile( pFilStrm->hFile, pBuf, dwNumberOfBytesToRead, pBufsize, NULL );
	if( fRes == FALSE )
	{
		*pErr = cdSTREAM_READ_ERROR;
		return;
	}
	
}

/* The WRITE function registered into a stream */
void cdSTDCALL _WriteMyFilStream(	cdContext	contextH,
									const void	*pBuf,
									cdUInt32	*pBufsize,
									cdError		*pErr )
{
	FilStreamData	*pFilStrm;
	BOOL			fRes;
	DWORD			dwNumberOfBytesToWrite;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_WRITE_ERROR;
		return;
	}
	
	dwNumberOfBytesToWrite = *pBufsize;
	fRes = WriteFile( pFilStrm->hFile, pBuf, dwNumberOfBytesToWrite, pBufsize, NULL );
	if( fRes ==FALSE )
	{
		*pErr = cdSTREAM_WRITE_ERROR;
		return;
	}
	
}

/* The SEEK function registered into a stream */
void cdSTDCALL _SeekMyFilStream(	cdContext	contextH,
									cdWhence	origin,
									cdInt32		offset,
									cdError		*pErr )
{
	FilStreamData	*pFilStrm;
	DWORD			dwMoveMethod;
	DWORD			dwLowPos;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_SEEK_ERROR;
		return;
	}
	
	switch( origin )
	{
		case cdSTART:
			dwMoveMethod = FILE_BEGIN;
			break;
		case cdCURRENT:
			dwMoveMethod = FILE_CURRENT;
			break;
		case cdEND:
			dwMoveMethod = FILE_END;
			break;
		default:
			*pErr = cdSTREAM_BAD_OPTIONS;
			return;
	}
	
	dwLowPos = SetFilePointer( pFilStrm->hFile, offset, NULL, dwMoveMethod );
	if( (dwLowPos==0xFFFFFFFF) && (GetLastError() != NO_ERROR) )
	{
		*pErr = cdSTREAM_SEEK_ERROR;
	}
	
}

/* The TELL function registered into a stream */
cdInt32 cdSTDCALL _TellMyFilStream(	cdContext	contextH,
									cdError		*pErr )
{
	FilStreamData	*pFilStrm;
	cdInt32			dwLowPos;
	
	
	*pErr = cdOK;
	/* It investigates whether data required for processing is assembled. */
	pFilStrm = (FilStreamData*)contextH;
	if( !pFilStrm )
	{
		*pErr = cdSTREAM_TELL_ERROR;
		return	0;
	}
	
	dwLowPos = SetFilePointer( pFilStrm->hFile, 0, NULL, FILE_CURRENT );
	if( (dwLowPos==0xFFFFFFFF) && (GetLastError() != NO_ERROR) )
	{
		*pErr = cdSTREAM_SEEK_ERROR;
		return	0;
	}
	
	return	dwLowPos;
}

