// Progress.cpp : implementation file
//

#include "stdafx.h"
#include "RelCtrl.h"
#include "Progress.h"
#include "MyStream.h"

#include "ComSettingDlg.h"

#include "prAPI.h"
#include "cdAPI.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#ifndef		GETERRORID
#define		GETERRORID( x )		(cdERROR_ERRORID_MASK&x)
#endif


/////////////////////////////////////////////////////////////////////////////
// CProgress dialog


CProgress::CProgress(CWnd* pParent /*=NULL*/)
	: CDialog(CProgress::IDD, pParent)
{
	//{{AFX_DATA_INIT(CProgress)
	//}}AFX_DATA_INIT
	m_ReleaseThread = NULL;
	m_ReleaseThread_CDSDK = NULL;
}


void CProgress::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProgress)
	DDX_Control(pDX, IDC_PROGRESS, m_CProgress);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProgress, CDialog)
	//{{AFX_MSG_MAP(CProgress)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProgress Internal function

/* The definition of the callback function for progress */
prResponse prSTDCALL CProgress::ReleaseProgressFunc(	prHandle       CameraHandle,
														prObjectHandle ObjectHandle,
														prContext      Context,
														prProgress*    pProgress	)
{
	CProgress		*CpThis;

	CpThis = (CProgress*)Context;

	/* save image data into a file */
	try
	{
		switch( pProgress->lMessage )
		{
			case prMSG_DATA_HEADER:
				/* Open a file */
				CpThis->m_pCFile = new CFile(	(LPCTSTR)CpThis->m_szFileName,
												(CFile::modeCreate | CFile::modeWrite)	);
				break;
			case prMSG_DATA:
				/* Move offset and save the size of data into a file */
				if( CpThis->m_pCFile )
				{
					(void)(CpThis->m_pCFile)->Seek(	(LONG)pProgress->lOffset,
													CFile::begin				);
					(CpThis->m_pCFile)->Write(	(const void*)pProgress->pbData,
												(UINT)pProgress->lLength		);
					CpThis->SetProgressPos( pProgress->lPercentComplete );
				}
				break;
			case prMSG_TERMINATION:
				/* close a file */
				if( CpThis->m_pCFile )
				{
					delete CpThis->m_pCFile;
					CpThis->m_pCFile = NULL;
				}
				break;
		}
	}
	catch( CFileException e )	/* Error in file save */
	{
		if( CpThis->m_pCFile )
		{
			delete CpThis->m_pCFile;
			CpThis->m_pCFile = NULL;
		}
		return prOPERATION_CANCELLED;	/* If a error is occured in image data transfer, */
	}					/* then return prOPERATION_CANCELLED */

	return prOK;
}

/* The function performed by the thread */
UINT 	CProgress::ReleaseThreadProc( LPVOID	lpParameter )
{
	char			szExt[MAX_PATH],szPrefix[MAX_PATH];
	static	int		j=0;
	CProgress		*CpThis;

	CpThis = (CProgress*)lpParameter;

	/* Determine the extension from the image type */
	if( CpThis->m_SavedFileFormat == prPTP_EXIF_JPEG )
	{
		strcpy(szExt, "JPG");
	}
	else if( CpThis->m_SavedFileFormat == prPTP_CRW )
	{
		if( CpThis->m_EventCode == prPTP_THUMBNAIL_RELEASED )	/* Thumbnail */
		{
			strcpy(szExt, "JPG");
		}
		else if( CpThis->m_EventCode == prPTP_FULL_VIEW_RELEASED ) /* Main image */
		{
			strcpy(szExt, "CRW");
		}
	}

	/* determine the prefix from the event code */
	if( CpThis->m_EventCode == prPTP_THUMBNAIL_RELEASED )	/* Thumbnail */
	{
		strcpy(szPrefix, "TH");
	}
	else if( CpThis->m_EventCode == prPTP_FULL_VIEW_RELEASED ) /* Main image */
	{
		strcpy(szPrefix, "FV");
	}

	/* Determine the name of the image data which is saved */
	wsprintf( CpThis->m_szFileName, "%s%s%06d.%s", CpThis->m_szSavePath, szPrefix, j, szExt );
	CpThis->m_pCFile = NULL;

	CpThis->m_LastErr = PR_RC_GetReleasedData(	CpThis->m_CameraHandle,
												CpThis->m_ObjectHandle,
												CpThis->m_EventCode,
												(1024L*1024L),			/* You can change this parameter. */
												(prContext)CpThis,
												(prGetFileDataCB*)&ReleaseProgressFunc	);
	j++;
	CpThis->PostMessage( CpThis->m_ThreadEndMessage );
	return	0;
}

void CProgress::SetProgressPos( int		iPos )
{
	m_CProgress.SetPos( iPos );
}

/////////////////////////////////////////////////////////////////////////////
// CProgress message handlers

LRESULT CProgress::WindowProc(UINT message, WPARAM wParam, LPARAM lParam) 
{
	// TODO: Add your specialized code here and/or call the base class
	if( m_ThreadEndMessage == message )		/* PS-ReC SDK */
	{
		if( m_ReleaseThread )
		{
			WaitForSingleObject( m_ReleaseThread->m_hThread,INFINITE );
			m_ReleaseThread = NULL;
		}
		EndDialog( IDOK );
		return	TRUE;
	}
	else if( m_ThreadEndMessage_CDSDK == message )	/* CD-SDK */
	{
		if( m_ReleaseThread_CDSDK )
		{
			WaitForSingleObject( m_ReleaseThread_CDSDK->m_hThread,INFINITE );
			m_ReleaseThread_CDSDK = NULL;
		}
		EndDialog( IDOK );
		return	TRUE;
	}
	
	return CDialog::WindowProc(message, wParam, lParam);
}

BOOL CProgress::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here
	
	/* Progress control is changed the first stage. */
	m_CProgress.SetRange( 0, 100 );
	SetProgressPos( 0 );
	
	/* Negatives are Release using a thread. */
	if(	m_SelectedSDK == CAMERA_ENUM_PRSDK	)
	{
		m_LastErr = prOK;
		m_ReleaseThread = AfxBeginThread(	(AFX_THREADPROC)ReleaseThreadProc,
											(LPVOID)this );
	}
	else	/* m_SelectedSDK == CAMERA_ENUM_CDSDK */
	{
		m_LastErr_CDSDK = cdOK;
		m_ReleaseThread_CDSDK = AfxBeginThread(	(AFX_THREADPROC)ReleaseThreadProc_CDSDK,
												(LPVOID)this );
	}
	return TRUE;  // return TRUE  unless you set the focus to a control
}

BOOL	CProgress::GetReleaseData(	prHandle        		CameraHandle,
									prObjectHandle  		ObjectHandle,
									prptpEventCode  		EventCode,
									prptpObjectFormatCode	SavedFileFormat,
									char					*szSavePath)
{
	/* Data is set. */
	m_SelectedSDK     = CAMERA_ENUM_PRSDK;
	m_CameraHandle    = CameraHandle;
	m_ObjectHandle    = ObjectHandle;
	m_EventCode       = EventCode;
	m_SavedFileFormat = SavedFileFormat;
	strcpy( m_szSavePath, szSavePath );

	m_ThreadEndMessage = RegisterWindowMessage( "Information of a thread end" );
	if( m_ThreadEndMessage == 0 )
	{
		return	FALSE;
	}
	
	/* A progress dialog is displayed and developed. */
	if( DoModal() != IDOK )
	{
		return	FALSE;
	}
	
	return	TRUE;
}

/////////////////////////////////////////////////////////////////////////////
//  CD-SDK function

/* The definition of the callback function for progress */
cdUInt32 cdSTDCALL CProgress::ReleaseProgressFunc_CDSDK(	cdUInt32			Progress,
															cdProgressStatus	Status,
															cdContext			Context )
{
	CProgress	*CpThis;

	CpThis = (CProgress*)Context;
	CpThis->SetProgressPos( Progress );
	return	cdOK;
}

/* The function performed by the thread */
UINT 	CProgress::ReleaseThreadProc_CDSDK( LPVOID	lpParameter )
{
	cdStgMedium					MyMedium;
	cdReleaseImageInfo			RelImgInfo;
	char						szFileName[MAX_PATH],szExt[MAX_PATH],szPrefix[MAX_PATH];
	static	int					i=0;
	CProgress					*CpThis;
	cdUInt32					NumData;
	cdStream					FilStream;
	
	CpThis = (CProgress*)lpParameter;
		
	for( NumData=CpThis->m_NumData_CDSDK; NumData>0; NumData--, i++ )
	{
		/* A preservation file name is decided. */
		if( CDGetReleasedData(	CpThis->m_hSource_CDSDK,
								NULL,
								0,
								0,
								&RelImgInfo,
								NULL ) != cdOK )
		{
			break;
		}
		
		switch( RelImgInfo.DataType )
		{
			case cdDATA_TYPE_THUMBNAIL:
				strcpy(szPrefix, "TH");
				strcpy(szExt, "JPG");
				break;
			case cdDATA_TYPE_PICTURE:
				strcpy(szPrefix, "FV");
				if(RelImgInfo.Format == 1)
				{
					strcpy(szExt, "JPG");
				}
				else
				{
					strcpy(szExt, "CRW");
				}
				break;
			case cdDATA_TYPE_PLUS_JPEG:
				strcpy(szPrefix, "FP");
				strcpy(szExt, "JPG");
				break;
		}
		
		wsprintf( szFileName, "%s%s%06d.%s",
				CpThis->m_szSavePath, szPrefix, i, szExt );
		
		/* A stream is initialized. */
		CreateMyFilStream( &FilStream, szFileName );
			
		MyMedium.Type = cdMEMTYPE_STREAM;
		MyMedium.u.pStream = &FilStream;
		/* Data is acquired. */
		CpThis->m_LastErr_CDSDK = CDGetReleasedData(	CpThis->m_hSource_CDSDK,
														ReleaseProgressFunc_CDSDK,
														(cdContext)CpThis,
														cdPROG_REPORT_PERIODICALLY,
														&RelImgInfo,
														&MyMedium );
		/* A stream is released. */
		ReleaseMyFilStream( &FilStream );
		if( GETERRORID(CpThis->m_LastErr_CDSDK) != cdOK )
		{
			break;
		}
	}
	
	CpThis->PostMessage( CpThis->m_ThreadEndMessage_CDSDK );
	return	0;
}

BOOL	CProgress::GetReleaseData_CDSDK(	cdHSource	hSource,
											cdUInt32	NumData,
											char		*szSavePath )
{
	/* Data is set. */
	m_SelectedSDK   = CAMERA_ENUM_CDSDK;
	m_hSource_CDSDK = hSource;
	m_NumData_CDSDK = NumData;
	strcpy( m_szSavePath, szSavePath );
	
	m_ThreadEndMessage_CDSDK = RegisterWindowMessage( "Information of a thread end for CD-SDK" );
	if( m_ThreadEndMessage_CDSDK == 0 )
	{
		return	FALSE;
	}
	
	/* A progress dialog is displayed and developed. */
	if( DoModal() != IDOK )
	{
		return	FALSE;
	}
	
	return	TRUE;
}

