// RelCtrlDlg.cpp : implementation file
//

#include "stdafx.h"
#include <vfw.h>
#include <afxmt.h>

#include "RelCtrl.h"
#include "Progress.h"
#include "RelCtrlDlg.h"
#include "ComSettingDlg.h"

#include "cdAPI.h"

#include "prType.h"
#include "prError.h"
#include "prAPI.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRelCtrlDlg dialog

CRelCtrlDlg::CRelCtrlDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CRelCtrlDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CRelCtrlDlg)
	m_CInfoString = _T("");
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32

	/*	WindowMessageID Reset	*/
	m_ReleaseOnMessage = 0;
	m_ReleaseCompleteMessage = 0;
	m_AbortPCEVF = 0;
	m_ChangeByUI = 0;

	m_FileType = 0;
	m_hCamera = 0L;
	m_CpVFThread = NULL;
	m_PicHandle = 0;
	m_ThmbHandle = 0;
	m_fGetRelease = FALSE;
	m_fInReleaseControl = FALSE;

	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CRelCtrlDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CRelCtrlDlg)
	DDX_Control(pDX, IDC_STATIC_SHOOTING_MODE, m_Static_ShootingMode);
	DDX_Control(pDX, IDC_STATIC_RELEASE_PARAM, m_Static_ReleaseParam);
	DDX_Control(pDX, IDC_STATIC_PHOTO_EFFECT, m_Static_PhotoEffect);
	DDX_Control(pDX, IDC_STATIC_ISO_SPEED, m_Static_ISOSpeed);
	DDX_Control(pDX, IDC_STATIC_FLASH, m_Static_Flash);
	DDX_Control(pDX, IDC_STATIC_EXPO_COMP, m_Static_ExpoComp);
	DDX_Control(pDX, IDC_PHOTO_EFFECT, m_CPhotoEffect);
	DDX_Control(pDX, IDC_SHOOTING_MODE, m_CShootingMode);
	DDX_Control(pDX, IDC_ISO_SPEED, m_CISOSpeed);
	DDX_Control(pDX, IDC_FLASH, m_CFlash);
	DDX_Control(pDX, IDC_EXPO_COMP, m_CExpoComp);
	DDX_Control(pDX, IDC_SAVEPATH, m_CSavePath);
	DDX_Control(pDX, IDC_BROWSE, m_CBrowse);
	DDX_Control(pDX, IDC_AEAF, m_CAEAF);
	DDX_Control(pDX, IDC_VIEWIMG, m_CViewImage);
	DDX_Control(pDX, IDC_VIEWFINDER, m_CViewFinder);
	DDX_Control(pDX, IDC_RELEASE, m_CRelease);
	DDX_Control(pDX, IDC_RELCHECK4, m_CRelChk4);
	DDX_Control(pDX, IDC_RELCHECK3, m_CRelChk3);
	DDX_Control(pDX, IDC_RELCHECK2, m_CRelChk2);
	DDX_Control(pDX, IDC_END, m_CEnd);
	DDX_Control(pDX, IDC_DISCONNECT, m_CDisconnect);
	DDX_Control(pDX, IDC_CONNECT, m_CConnect);
	DDX_Text(pDX, IDC_INFORMATION, m_CInfoString);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CRelCtrlDlg, CDialog)
	//{{AFX_MSG_MAP(CRelCtrlDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_CONNECT, OnConnect)
	ON_BN_CLICKED(IDC_DISCONNECT, OnDisconnect)
	ON_BN_CLICKED(IDC_END, OnEnd)
	ON_BN_CLICKED(IDC_RELEASE, OnRelease)
	ON_BN_CLICKED(IDC_RELCHECK2, OnRelcheck2)
	ON_BN_CLICKED(IDC_RELCHECK3, OnRelcheck3)
	ON_BN_CLICKED(IDC_RELCHECK4, OnRelcheck4)
	ON_BN_CLICKED(IDC_VIEWFINDER, OnViewfinder)
	ON_BN_CLICKED(IDC_AEAF, OnAeaf)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	ON_WM_DESTROY()
	ON_CBN_SELCHANGE(IDC_SHOOTING_MODE, OnSelchangeShootingMode)
	ON_CBN_SELCHANGE(IDC_EXPO_COMP, OnSelchangeExpoComp)
	ON_CBN_SELCHANGE(IDC_FLASH, OnSelchangeFlash)
	ON_CBN_SELCHANGE(IDC_ISO_SPEED, OnSelchangeISOSpeed)
	ON_CBN_SELCHANGE(IDC_PHOTO_EFFECT, OnSelchangePhotoEffect)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Call back function

prResponse prSTDCALL CRelCtrlDlg::CamCallBackFunc(prHandle		CameraHandle,
										prContext		Context,
										prVoid*			pEventData)
{
	CRelCtrlDlg		*CpThis;
	EVENT_GENERIC_CONTAINER		*pEventDataTemp;
		
	CpThis = (CRelCtrlDlg*)Context;
	pEventDataTemp = (EVENT_GENERIC_CONTAINER *)pEventData;

	switch ( pEventDataTemp->Code )
	{
		case prCAL_SHUTDOWN:
			CpThis->PostMessage(CpThis->m_CameraShutDown);
			break;
		case prPTP_ABORT_PC_EVF:
			CpThis->PostMessage(CpThis->m_AbortPCEVF);
			break;
		case prPTP_FULL_VIEW_RELEASED:
			CpThis->m_PicHandle = pEventDataTemp->Parameter[0];
			CpThis->m_ReleaseComp |= FULL_VIEW_REL;
			break;
		case prPTP_THUMBNAIL_RELEASED:
			CpThis->m_ThmbHandle = pEventDataTemp->Parameter[0];
			CpThis->m_ReleaseComp |= THMB_VIEW_REL;
			break;
		case prPTP_CAPTURE_COMPLETE:
			break;
		case prPTP_PUSHED_RELEASE_SW:
			CpThis->PostMessage( CpThis->m_ReleaseOnMessage );
			break;
		case prPTP_RC_PROP_CHANGED:
		case prPTP_DEVICE_PROP_CHANGED:
			CpThis->PostMessage( CpThis->m_ChangeByUI );
			break;
	}
	
	return	prOK;
}

prResponse prSTDCALL CRelCtrlDlg::ViewFinderCallBackFun(prHandle		CameraHandle,
														 prContext		Context,
														 prUInt32		Size,
														 prVoid*		pEventData)
{
	CRelCtrlDlg		*CpThis;

	CpThis = (CRelCtrlDlg*)Context;

	CpThis->m_CVFMutex.Lock();

	memset( &CpThis->m_ViewFinderData[0],0,VF_BUFFER_SIZE );

	/* Copy the image data */
	memcpy( &CpThis->m_ViewFinderData[0],(prUInt8*)pEventData,Size );
	CpThis->m_VF_DataSize = Size;

	CpThis->m_CVFMutex.Unlock();

	if ( CpThis->m_CpVFThread )
	{
		/* Resume the thread for displaying a picture */
		CpThis->m_CpVFThread->ResumeThread();
	}
	return prOK;
}

/////////////////////////////////////////////////////////////////////////////
// Thread function

/* The thread for displaying a picture */
UINT WINAPI	CRelCtrlDlg::ViewFinderProc( LPVOID	vpParam )
{
	BOOL			*fpEnd;
	HDC				hdcDest;
	prUInt8			cBmpVuffer[VIEWFINDER_WIDTH * VIEWFINDER_HEIGHT * 3];
	prUInt8*		cpBM_Buffer;
	prUInt8*		cpVF_Buffer;
	prUInt32		lCnt;
	prUInt32		SavePoint;
	prUInt32*		cpVF_Size;
	CRelCtrlDlg		*CpThis;

	BITMAPINFO	bmiDIB;


	CpThis = (CRelCtrlDlg*)vpParam;
	fpEnd = (BOOL*)(&CpThis->m_fVFEnd);

	cpVF_Buffer = &CpThis->m_ViewFinderData[0];
	cpVF_Size = &CpThis->m_VF_DataSize;

	memset( &bmiDIB, 0, sizeof(bmiDIB) );
	bmiDIB.bmiHeader.biSize				= sizeof(BITMAPINFOHEADER);
	bmiDIB.bmiHeader.biWidth			= VIEWFINDER_WIDTH;
	bmiDIB.bmiHeader.biHeight			= VIEWFINDER_HEIGHT * -1;
	bmiDIB.bmiHeader.biPlanes			= 1;
	bmiDIB.bmiHeader.biBitCount			= 24;
	bmiDIB.bmiHeader.biCompression		= BI_RGB;

	while ( *fpEnd )
	{
		CpThis->m_CVFMutex.Lock();

		/* A picture is displayed. */
		hdcDest = ::GetDC( CpThis->m_CpViewFinder->GetSafeHwnd() );

		/* Initiate the Jpeg library */
		memset( &CpThis->m_Proj, 0, sizeof(CpThis->m_Proj) );
		CpThis->m_Proj.cinfo.err	= jpeg_std_error( &CpThis->m_Proj.jerr);
		jpeg_create_decompress( &CpThis->m_Proj.cinfo);

		memset( &cBmpVuffer[0], 0, sizeof(cBmpVuffer));

		jpeg_stdio_src( &CpThis->m_Proj.cinfo, (FILE *)cpVF_Buffer, *cpVF_Size);
		/* Read JPEG header */
		jpeg_read_header( &CpThis->m_Proj.cinfo, TRUE);
		jpeg_start_decompress( &CpThis->m_Proj.cinfo);

		for ( lCnt = 0;lCnt < CpThis->m_Proj.cinfo.image_height;lCnt++ )
		{
			SavePoint = lCnt * 3 * VIEWFINDER_WIDTH;
			cpBM_Buffer = &cBmpVuffer[SavePoint];
			/* Convert JPEG into BMP */
			jpeg_read_scanlines( &CpThis->m_Proj.cinfo, &cpBM_Buffer, 1);
		}

		/* The process for displaying a picture */
		::SetDIBitsToDevice(
			hdcDest,
			0, 0, VIEWFINDER_WIDTH, VIEWFINDER_HEIGHT,
			0, 0,
			0, VIEWFINDER_HEIGHT,
			&cBmpVuffer[0], &bmiDIB,
			DIB_RGB_COLORS
		);
		
		jpeg_finish_decompress( &CpThis->m_Proj.cinfo);

		// finish the Jpeg library
		jpeg_destroy_decompress( &CpThis->m_Proj.cinfo);

		::ReleaseDC( CpThis->m_CpViewFinder->GetSafeHwnd(), hdcDest );

		CpThis->m_CVFMutex.Unlock();

		/* A thread is suspended. */
		CpThis->m_CpVFThread->SuspendThread();

	}

	return	0;
}


/////////////////////////////////////////////////////////////////////////////
// CRelCtrlDlg Internal function

/* DIB is created. */
BOOL	CRelCtrlDlg::CreateDIBBuffer()
{
	cdError		err=cdOK;
	BITMAPINFO	BmpInfo;

	memset( &BmpInfo, 0, sizeof(BITMAPINFO) );
	BmpInfo.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	BmpInfo.bmiHeader.biWidth = VIEWFINDER_WIDTH;
	BmpInfo.bmiHeader.biHeight = VIEWFINDER_HEIGHT;
	BmpInfo.bmiHeader.biPlanes = 1;
	BmpInfo.bmiHeader.biBitCount = 24;
	BmpInfo.bmiHeader.biCompression = BI_RGB;

	m_BackSurface.hBmp = ::CreateDIBSection(	NULL,
												&BmpInfo,
												DIB_RGB_COLORS,
												&m_BackSurface.vpBits,
												NULL,
												0	);
	if( !m_BackSurface.hBmp )
	{
		return	FALSE;
	}
	
	return	TRUE;
}

void	CRelCtrlDlg::GetCameraInformation()
{

	CString		CAddStr;
	prResponse	cErr = prOK;
	prUInt8		cValue8;
	prUInt32	BufferSize = 0L;
	prUInt32	Num;

	UpdateData( TRUE );
	m_CInfoString = "";

	/* Quality is acquired. */
	BufferSize = sizeof(cValue8);
	cErr = PR_GetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_COMP_QUALITY,
									&BufferSize,
									&cValue8	);
	if ( cErr == prOK )
	{
		switch ( cValue8 )
		{
			case 0x01:
				CAddStr.Format( "ImageQuality = Economy\r\n" );
				break;
			case 0x02:
				CAddStr.Format( "ImageQuality = Normal\r\n" );
				break;
			case 0x03:
				CAddStr.Format( "ImageQuality = Fine\r\n" );
				break;
			case 0x04:
				CAddStr.Format( "ImageQuality = Lossless\r\n" );
				break;
			case 0x05:
				CAddStr.Format( "ImageQuality = SuperFine\r\n" );
				break;
			default:
				CAddStr.Format( "ImageQuality = Unknown\r\n" );
				break;
		}
		m_CInfoString += CAddStr;
	}
	else
	{
		CAddStr.Format( "<err>prPTP_DEV_PROP_COMP_QUALITY = 0x%08X\r\n", cErr );
		m_CInfoString += CAddStr;
	}

	/* ImageSize is acquired. */
	BufferSize = sizeof(cValue8);
	cErr = PR_GetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_IMAGE_SIZE,
									&BufferSize,
									&cValue8	);
	if ( cErr == prOK )
	{
		switch ( cValue8 )
		{
			case 0x00:
				CAddStr.Format( "ImageSize = Large\r\n" );
				break;
			case 0x01:
				CAddStr.Format( "ImageSize = Medium1\r\n" );
				break;
			case 0x02:
				CAddStr.Format( "ImageSize = Small\r\n" );
				break;
			case 0x03:
				CAddStr.Format( "ImageSize = Medium2\r\n" );
				break;
			case 0x07:
				CAddStr.Format( "ImageSize = Medium3\r\n" );
				break;
			default:
				CAddStr.Format( "ImageSize = Unknown\r\n" );
				break;
		}
		m_CInfoString += CAddStr;
	}
	else
	{
		CAddStr.Format( "<err>prPTP_DEV_PROP_IMAGE_SIZE = 0x%08X\r\n", cErr );
		m_CInfoString += CAddStr;
	}

	/* Shooting mode is acquired. */
	BufferSize = sizeof(cValue8);
	cErr = PR_GetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_EXPOSURE_MODE,
									&BufferSize,
									&cValue8	);
	if ( cErr == prOK )
	{
		switch ( cValue8 )
		{
			case 0x00:
				CAddStr.Format( "ShootingMode = Auto\r\n" );
				break;
			case 0x01:
				CAddStr.Format( "ShootingMode = P\r\n" );
				break;
			case 0x02:
				CAddStr.Format( "ShootingMode = Tv\r\n" );
				break;
			case 0x03:
				CAddStr.Format( "ShootingMode = Av\r\n" );
				break;
			case 0x04:
				CAddStr.Format( "ShootingMode = M\r\n" );
				break;
			case 0x05:
				CAddStr.Format( "ShootingMode = A_DEP\r\n" );
				break;
			case 0x06:
				CAddStr.Format( "ShootingMode = M_DEP\r\n" );
				break;
			case 0x07:
				CAddStr.Format( "ShootingMode = Bulb\r\n" );
				break;
			case 0x80:
				CAddStr.Format( "ShootingMode = CAMERAM\r\n" );
				break;
			case 0x81:
				CAddStr.Format( "ShootingMode = MYCOLOR\r\n" );
				break;
			case 0x82:
				CAddStr.Format( "ShootingMode = PORTRAIT\r\n" );
				break;
			case 0x83:
				CAddStr.Format( "ShootingMode = LANDSCAPE\r\n" );
				break;
			case 0x84:
				CAddStr.Format( "ShootingMode = NIGHTSCENE\r\n" );
				break;
			case 0x85:
				CAddStr.Format( "ShootingMode = FOREST\r\n" );
				break;
			case 0x86:
				CAddStr.Format( "ShootingMode = SNOW\r\n" );
				break;
			case 0x87:
				CAddStr.Format( "ShootingMode = BEACH\r\n" );
				break;
			case 0x88:
				CAddStr.Format( "ShootingMode = FIREWORKS\r\n" );
				break;
			case 0x89:
				CAddStr.Format( "ShootingMode = PARTY\r\n" );
				break;
			case 0x8a:
				CAddStr.Format( "ShootingMode = NIGHTSNAP\r\n" );
				break;
			case 0x8b:
				CAddStr.Format( "ShootingMode = STITCH\r\n" );
				break;
			case 0x8c:
				CAddStr.Format( "ShootingMode = MOVIE\r\n" );
				break;
			case 0x8d:
				CAddStr.Format( "ShootingMode = CUSTOM\r\n" );
				break;
			case 0x8e:
				CAddStr.Format( "ShootingMode = INTERVAL\r\n" );
				break;
			case 0x8f:
				CAddStr.Format( "ShootingMode = DIGITALMACRO\r\n" );
				break;
			case 0x90:
				CAddStr.Format( "ShootingMode = LONGSHUTTER\r\n" );
				break;
			case 0x91:
				CAddStr.Format( "ShootingMode = UNDERWATER\r\n" );
				break;
			case 0x92:
				CAddStr.Format( "ShootingMode = KIDSANDPETS\r\n" );
				break;
			case 0x93:
				CAddStr.Format( "ShootingMode = FASTSHUTTER\r\n" );
				break;
			case 0x94:
				CAddStr.Format( "ShootingMode = SLOWSHUTTER\r\n" );
				break;
			case 0x95:
				CAddStr.Format( "ShootingMode = CUSTOM1\r\n" );
				break;
			case 0x96:
				CAddStr.Format( "ShootingMode = CUSTOM2\r\n" );
				break;
			case 0x97:
				CAddStr.Format( "ShootingMode = NEUTRAL\r\n" );
				break;
			case 0x98:
				CAddStr.Format( "ShootingMode = GRAY\r\n" );
				break;
			case 0x99:
				CAddStr.Format( "ShootingMode = SEPIA\r\n" );
				break;
			case 0x9A:
				CAddStr.Format( "ShootingMode = VIVID\r\n" );
				break;
			case 0x9B:
				CAddStr.Format( "ShootingMode = SPORTS\r\n" );
				break;
			case 0x9C:
				CAddStr.Format( "ShootingMode = MACRO\r\n" );
				break;
			case 0x9D:
				CAddStr.Format( "ShootingMode = SUPERMACRO\r\n" );
				break;
			case 0x9E:
				CAddStr.Format( "ShootingMode = PANFOCUS\r\n" );
				break;
			case 0x9F:
				CAddStr.Format( "ShootingMode = BW\r\n" );
				break;
			case 0xA0:
				CAddStr.Format( "ShootingMode = FLASHINHIBIT\r\n" );
				break;
			default:
				CAddStr.Format( "ShootingMode = (Error)\r\n" );
				break;
		};
		m_CInfoString += CAddStr;
	}
	else
	{
		CAddStr.Format( "<err>prPTP_DEV_PROP_EXPOSURE_MODE = 0x%08X\r\n", cErr );
		m_CInfoString += CAddStr;
	}

	/* The number of sheets which can be remaining photoed is acquired. */
	cErr = PR_RC_GetNumAvailableShot( m_hCamera, &Num );
	if ( cErr == prOK )
	{
		CAddStr.Format( "NumAvailableShot = %d\r\n", Num );
		m_CInfoString += CAddStr;
	}
	else
	{
		CAddStr.Format( "<err>PR_RC_GetNumAvailableShot = 0x%08X\r\n", cErr );
		m_CInfoString += CAddStr;
	}

	UpdateData( FALSE );
}

/* The state of a Release button is set up. */
prResponse	CRelCtrlDlg::SetReleaseState()
{
	cdError			cErr = cdOK;
	prUInt16		wTransMode = 0;
	prUInt32		DataSize;

	if ( m_CRelChk2.GetCheck() == 1 )
	{
		wTransMode |= 0x0002;
	}
	if ( m_CRelChk3.GetCheck() == 1 )
	{
		wTransMode |= 0x0004;
	}
	if ( m_CRelChk4.GetCheck() == 1 )
	{
		wTransMode |= 0x0008;
	}

	DataSize = sizeof(wTransMode);
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE,
									DataSize,
									&wTransMode	);
	return cErr;
}

void CRelCtrlDlg::GetSavePath(	char	*cpBuffer,
								int		iBufferSize )
{
	char	*cpPos;
	
	
	/* The directory to save is acquired. */
	m_CSavePath.GetWindowText( cpBuffer, iBufferSize );
	if( strlen(cpBuffer) == 0 )
	{
		GetModuleFileName( NULL, cpBuffer, iBufferSize );
		cpPos = strrchr( cpBuffer, '\\' );
		if( cpPos )
		{
			*(cpPos+1) = '\0';
		}
	}
	else
	{
		if( cpBuffer[strlen(cpBuffer)-1] != '\\' )
		{
			strcat( cpBuffer, "\\" );
		}
	}
	
}


/////////////////////////////////////////////////////////////////////////////
// CRelCtrlDlg message handlers

BOOL CRelCtrlDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);
	
	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if( pSysMenu != NULL )
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if(!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}
	
	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here
	BOOL		fRes;
	char		szPath[MAX_PATH];
	char		*cpPos;
	cdError		err;
	prResponse	cErr;
	char	szErrStr[256];

	/* CDSDK Area Initialize */
	m_hSource = NULL;
	m_hCallbackFunction = NULL;
	m_CpViewFinder = &m_CViewImage;
	memset( &m_BackSurface, 0, sizeof(DIBData) );
	m_fProgramRelease = FALSE;
	m_fCamTypeEOS = FALSE;
	m_fMCard = FALSE;
	m_fVFEnd = FALSE;

	/* PS-Rec SDK Area Initialize */
	m_hCamera = 0L;
	m_CpVFThread = NULL;
	m_fGetRelease = FALSE;

	/* A picture buffer is created. */
	fRes = CreateDIBBuffer();
	if( !fRes )
	{
		goto	apierr;
	}

	/* A message is registered. */
	m_ReleaseOnMessage = RegisterWindowMessage( CAMERAEVENTMESSAGESTRING_RELEASE_ON );
	m_ReleaseCompleteMessage = RegisterWindowMessage( CAMERAEVENTMESSAGESTRING_RELEASE_COMP );
	m_AbortPCEVF = RegisterWindowMessage( CAMERAEVENTMESSAGESTRING_ABORT_PC_EVF );
	m_ChangeByUI = RegisterWindowMessage( CAMERAEVENTMESSAGESTRING_CHANGE_BY_UI );
	m_CameraShutDown = RegisterWindowMessage(CAMERAEVENTMESSAGESTRING_SHUT_DOWN);

	if (	!m_ReleaseOnMessage
		||	!m_ReleaseCompleteMessage
		||	!m_AbortPCEVF
		||	!m_ChangeByUI
		||	!m_CameraShutDown 	)
	{
		goto	apierr;
	}

	/* Dialog controls are initialized. */
	m_CConnect.EnableWindow( TRUE );
	m_CDisconnect.EnableWindow( FALSE );
	m_CRelease.EnableWindow( FALSE );
	m_CViewFinder.EnableWindow( FALSE );
	m_CAEAF.EnableWindow( FALSE );	
	m_CRelChk2.EnableWindow( FALSE );
	m_CRelChk3.EnableWindow( FALSE );
	m_CRelChk4.EnableWindow( FALSE );
	
	m_CRelChk2.SetCheck( 1 );
	m_CRelChk3.SetCheck( 0 );
	m_CRelChk4.SetCheck( 0 );
	
	ShowReleaseParam(NULL);

	m_CViewImage.SetWindowPos(	NULL, 0, 0, 
								VIEWFINDER_WIDTH, VIEWFINDER_HEIGHT, 
								SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOZORDER );
	
	GetModuleFileName( NULL, szPath, MAX_PATH );
	cpPos = strrchr( szPath, '\\' );
	if( cpPos )
	{
		*(cpPos+1) = '\0';
	}
	m_CSavePath.SetWindowText( szPath );
	
	UpdateData(TRUE);
	m_CInfoString = "";
	UpdateData(FALSE);

	/* PS-Rec SDK START */
	cErr = PR_StartSDK();
	if ( cErr != prOK )
	{
		goto camerr;
	}

	/* CDSDK is changed the first stage. */
	cdVersionInfo	SVersion;
	memset( &SVersion, 0, sizeof(cdVersionInfo) );
	SVersion.Size = sizeof(cdVersionInfo);
	SVersion.MajorVersion = 7;
	SVersion.MinorVersion = 3;

	err = CDStartSDK( &SVersion, 0 );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}

	return TRUE;  // return TRUE  unless you set the focus to a control
	
apierr:
	MessageBox( "API Error" );
	EndDialog( 0 );
	return	FALSE;
	
camerr:

	if ( GETERRORID(err) != cdOK )
	{
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	}
	else if ( cErr != prOK )
	{
		wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	}

	MessageBox( szErrStr );
	EndDialog( 1 );
	return	FALSE;
}

void CRelCtrlDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if( (nID & 0xFFF0) == IDM_ABOUTBOX )
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CRelCtrlDlg::OnPaint() 
{
	if( IsIconic() )
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CRelCtrlDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CRelCtrlDlg::OnConnect() 
{
	// TODO: Add your control notification handler code here
	SDK_AND_INFO	cSelectInfo;
	CComSettingDlg* pComSettingDialog= NULL;
	prUInt32	BufferSize = 0L;
	prUInt8*	pDataBuffer = NULL;
	prUInt16	wMode = 0;
	prUInt16	wCurrentValue = 0;
	prBoolean	IsRC,IsEVF,IsAeaf,IsExpoMode,IsExpoComp,IsStrobeSet,IsISO,IsPhotoEffect,IsRet;
	prResponse	cErr = prOK;

	IsRC = FALSE;
	IsEVF = FALSE;
	IsAeaf = FALSE;
	IsExpoMode = FALSE;
	IsExpoComp = FALSE;
	IsStrobeSet = FALSE;
	IsISO = FALSE;
	IsPhotoEffect = FALSE;

	/* Create Buffer */
	pComSettingDialog = new CComSettingDlg( this );
	pComSettingDialog->SelectSource( &cSelectInfo );
	delete pComSettingDialog;

	if ( cSelectInfo.m_SelectedSDK == 0 )
	{
		return;
	}

	/* CDSDK */
	if ( cSelectInfo.m_SelectedSDK == CAMERA_ENUM_CDSDK )
	{
		if ( cSelectInfo.SrcInfo.SelectedSrc_CDSDK.SurceType == cdSRC_TYPE_INVALID )
		{
			return;
		} 
		OnConnect_CDSDK( &cSelectInfo.SrcInfo.SelectedSrc_CDSDK );
		return;
	}

	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];

	/* Making of camera steering wheel */
	cErr = PR_CreateCameraObject( &cSelectInfo.SrcInfo.SelectedSrc_PRSDK, &m_hCamera );
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}

	/* Registration of event callback function */
	cErr = PR_SetEventCallBack(	m_hCamera,
								(prContext)this,
								(prSetEventCB*)&CamCallBackFunc );
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}

	/* Connection of camera device */
	cErr = PR_ConnectCamera(m_hCamera);
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}

	/* Device information is acuired. */
	memset( pDataBuffer,0,MY_BUFFER_SIZE );
	BufferSize = MY_BUFFER_SIZE;
	cErr = PR_GetDeviceInfo(m_hCamera, &BufferSize, pDataBuffer);
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}

	/* Check if Release Control is supported */
	IsSupportedRCandEVF( pDataBuffer ,&IsRC ,&IsEVF ,&IsAeaf );
	if ( IsRC == FALSE )	/* not support Release Control */
	{
		goto ErrorFinish;
	}

	/* Release Control mode start */
	cErr = PR_InitiateReleaseControl( m_hCamera );
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}
	m_CRelease.EnableWindow( TRUE );
	m_fInReleaseControl = TRUE;

	if ( IsEVF == TRUE )		/* Support Viewfinder */
	{
		m_CViewFinder.EnableWindow( TRUE );
	}
	if ( IsAeaf == TRUE )	/* Support PR_RC_DoAeAfAwb */
	{
		m_CAEAF.EnableWindow( TRUE );
	}

	/* Device information is acuired. */
	memset( pDataBuffer, 0, MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	cErr = PR_GetDeviceInfo( m_hCamera, &BufferSize, pDataBuffer );
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}
	IsSupportedCapRelPrm( pDataBuffer,
						  &IsExpoMode,
						  &IsExpoComp,
						  &IsStrobeSet,
						  &IsISO,
						  &IsPhotoEffect );
	/* EXPOSURE_MODE is enable. */
	if ( IsExpoMode == TRUE )
	 {
		m_Static_ShootingMode.ShowWindow(SW_SHOW);
		m_CShootingMode.ShowWindow(SW_SHOW);
		m_Static_ReleaseParam.ShowWindow(SW_SHOW);
		SetShootingModeBox();
	 }
	/* EXPOSURE_COMP is enable. */
	if ( IsExpoComp == TRUE )
	{
		m_Static_ExpoComp.ShowWindow(SW_SHOW);
		m_CExpoComp.ShowWindow(SW_SHOW);
		m_Static_ReleaseParam.ShowWindow(SW_SHOW);
		SetExpoCompBox();
	}
	/* STROBE_SETTING is enable. */
	if ( IsStrobeSet == TRUE )
	{
		m_Static_Flash.ShowWindow(SW_SHOW);
		m_CFlash.ShowWindow(SW_SHOW);
		m_Static_ReleaseParam.ShowWindow(SW_SHOW);
		SetFlashBox();
	}
	/* ISO is enable. */
	if ( IsISO == TRUE )
	{
		m_Static_ISOSpeed.ShowWindow(SW_SHOW);
		m_CISOSpeed.ShowWindow(SW_SHOW);
		m_Static_ReleaseParam.ShowWindow(SW_SHOW);
		SetISOSpeedBox();
	}
	/* PHOTO_EFFECT is enable */
	if ( IsPhotoEffect == TRUE )
	{
		m_Static_PhotoEffect.ShowWindow(SW_SHOW);
		m_CPhotoEffect.ShowWindow(SW_SHOW);
		m_Static_ReleaseParam.ShowWindow(SW_SHOW);
		SetPhotoEffectBox();
	}

	/* CameraInformation */
	GetCameraInformation();

	/* Capture mode is acuired. */
	memset( pDataBuffer, 0, MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	cErr = PR_GetDevicePropDesc(	m_hCamera,
									prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE,
									&BufferSize,
									pDataBuffer	);
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}

	/* Check which mode is supported */
	IsRet = IsSuppotedCapTransMode( pDataBuffer,&wCurrentValue,&wMode );
	if ( IsRet == FALSE )
	{
		goto ErrorFinish;
	}

	/* Initiate check boxes */
	m_CRelChk2.SetCheck(0);
	m_CRelChk3.SetCheck(0);
	m_CRelChk4.SetCheck(0);
	if ( wMode & 0x0002 )
	{
		m_CRelChk2.EnableWindow(TRUE);
	}
	if ( wMode & 0x0004 )
	{
		m_CRelChk3.EnableWindow(TRUE);
	}
	if ( wMode & 0x0008 )
	{
		m_CRelChk4.EnableWindow(TRUE);
	}

	/* Set Default Capture Transfer Mode */
	if(wMode & 0x0002)
	{
		prUInt16	wTransMode = 0x0002;	/* Transfer Fullview Image to PC */
		cErr = PR_SetDevicePropValue(	m_hCamera,
										prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE,
										sizeof(wTransMode),
										&wTransMode	);
	}

	if ( wCurrentValue & 0x0002 )
	{
		m_CRelChk2.SetCheck(1);
	}
	if ( wCurrentValue & 0x0004 )
	{
		m_CRelChk3.SetCheck(1);
	}
	if ( wCurrentValue & 0x0008 )
	{
		m_CRelChk4.SetCheck(1);
	}

	/* Check the image type */
	memset( pDataBuffer,0,MY_BUFFER_SIZE );
	BufferSize = MY_BUFFER_SIZE;
	cErr = PR_GetDevicePropDesc(	m_hCamera,
									prPTP_DEV_PROP_FULLVIEW_FILE_FORMAT,
									&BufferSize,
									pDataBuffer	);
	if ( cErr != prOK )
	{
		goto ErrorFinish;
	}
	/* Check the image type for current shooting */
	IsRet = IsReleasePictureType( pDataBuffer );
	if ( IsRet == FALSE )
	{
		goto ErrorFinish;
	}

	m_CConnect.EnableWindow( FALSE );
	m_CDisconnect.EnableWindow( TRUE );

	/* Buffer Free */
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	return;

ErrorFinish:

	CameraDisconnect();

	/* Buffer Free */
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( cErr != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
		MessageBox( szErrStr );
	}
	else
	{
		MessageBox( "Not Supported or Data Error" );
	}
}

void CRelCtrlDlg::OnDisconnect() 
{
	// TODO: Add your control notification handler code here

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnDisconnect_CDSDK();
		return;
	}

	CameraDisconnect();

	m_CConnect.EnableWindow( TRUE );
	m_CDisconnect.EnableWindow( FALSE );
	m_CRelease.EnableWindow( FALSE );
	m_CViewFinder.EnableWindow( FALSE );
	m_CAEAF.EnableWindow( FALSE );
	m_CRelChk2.EnableWindow( FALSE );
	m_CRelChk3.EnableWindow( FALSE );
	m_CRelChk4.EnableWindow( FALSE );

	m_CShootingMode.ShowWindow(SW_HIDE);
	m_CExpoComp.ShowWindow(SW_HIDE);
	m_CFlash.ShowWindow(SW_HIDE);
	m_CISOSpeed.ShowWindow(SW_HIDE);
	m_CPhotoEffect.ShowWindow(SW_HIDE);

	m_Static_ShootingMode.ShowWindow(SW_HIDE);
	m_Static_ExpoComp.ShowWindow(SW_HIDE);
	m_Static_Flash.ShowWindow(SW_HIDE);
	m_Static_ISOSpeed.ShowWindow(SW_HIDE);
	m_Static_PhotoEffect.ShowWindow(SW_HIDE);
	m_Static_ReleaseParam.ShowWindow(SW_HIDE);

	UpdateData(TRUE);
	m_CInfoString = "";
	UpdateData(FALSE);
}

void CRelCtrlDlg::OnEnd() 
{
	// TODO: Add your control notification handler code here

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnEnd_CDSDK();
		return;
	}

	/* Disconnect the camera in PS-Rec SDK */
	CameraDisconnect();
	EndDialog( 0 );	
}

void CRelCtrlDlg::OnRelease() 
{
	// TODO: Add your control notification handler code here
	prResponse	cErr = prOK;
	char		cSavePath[MAX_PATH];
	BOOL		bRet = TRUE;
	CProgress*	cPhotProg = NULL;
	CProgress*	cThumbProg = NULL;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnRelease_CDSDK();
		return;
	}

	m_fGetRelease = TRUE;
	/* A photograph is taken. */
	cErr = PR_RC_Release( m_hCamera );
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	memset(&cSavePath,0,MAX_PATH);
	/* Save path is acuired. */
	GetSavePath(&cSavePath[0],MAX_PATH);

	if ( m_ReleaseComp & FULL_VIEW_REL )
	{
		cPhotProg = new CProgress;
		/* The data of the main image is acuired. */
		bRet = cPhotProg->GetReleaseData(	m_hCamera,
											m_PicHandle,
											prPTP_FULL_VIEW_RELEASED,
											m_FileType,
											&cSavePath[0]	);

		m_PicHandle = 0;
		cErr = cPhotProg->m_LastErr;
		delete cPhotProg;

		if ( bRet == FALSE )
		{
			m_ReleaseComp = 0;	
			goto SDK_Error;
		}
	}

	if ( m_ReleaseComp & THMB_VIEW_REL )
	{
		cThumbProg = new CProgress;
		/* The data of the thumbnail is acuired. */
		bRet = cThumbProg->GetReleaseData(	m_hCamera,
											m_ThmbHandle,
											prPTP_THUMBNAIL_RELEASED,
											m_FileType,
											&cSavePath[0]	);
		m_ThmbHandle = 0;
		cErr = cThumbProg->m_LastErr;
		delete cThumbProg;

		if ( bRet == FALSE )
		{
			m_ReleaseComp = 0;
			goto SDK_Error;
		}
	}

	m_ReleaseComp = 0;

	/* The information of number of shooting is updated. */
	GetCameraInformation();

	m_fGetRelease = FALSE;
	
	return;

SDK_Error:
	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );

	m_fGetRelease = FALSE;
}

void CRelCtrlDlg::OnRelcheck2() 
{
	// TODO: Add your control notification handler code here
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnRelcheck2_CDSDK();
		return;
	}

	cErr = SetReleaseState();
	if (cErr != prOK)
	{
		goto SDK_Error;
	}

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnRelcheck3() 
{
	// TODO: Add your control notification handler code here
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if (m_hSource)
	{
		OnRelcheck3_CDSDK();
		return;
	}

	cErr = SetReleaseState();
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnRelcheck4() 
{
	// TODO: Add your control notification handler code here
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnRelcheck4_CDSDK();
		return;
	}

	cErr = SetReleaseState();
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnViewfinder() 
{
	// TODO: Add your control notification handler code here
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnViewfinder_CDSDK();
		return;
	}
	/* UI is locked so that information may not be changed. */
	if ( !m_fVFEnd )
	{
		m_fVFEnd = TRUE;

		/* Start the thread for displaying a picutre */
		m_CpVFThread = AfxBeginThread(	(AFX_THREADPROC)ViewFinderProc,
										(LPVOID)this,
										0,
										0,
										CREATE_SUSPENDED,
										NULL	);

		/* Start Viewfinder */
		cErr = PR_RC_StartViewFinder(	m_hCamera,
										(prContext)this,
										(prViewFinderCB*)&ViewFinderCallBackFun	);
		if ( cErr != prOK )
		{
			goto Error;
		}

		m_CViewFinder.SetWindowText( "Stop" );

	}
	else
	{
		m_fVFEnd = FALSE;
		/* End the thread for displaying a picture */
		if ( m_CpVFThread )
		{
			m_CpVFThread->ResumeThread();
			WaitForSingleObject( m_CpVFThread->m_hThread, INFINITE );
			m_CpVFThread = NULL;
		}

		Invalidate();
		UpdateWindow();

		/* Terminate Viewfinder */
		PR_RC_TermViewFinder(m_hCamera);

		m_CViewFinder.SetWindowText( "Start" );	
	}

	return;

Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnAeaf() 
{
	prResponse				cErr = prOK;
	prptpAeAfAwbResetFlag	ActivateFlag = 0x00000007;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnAeaf_CDSDK();
		return;
	}

	/* AE and AF are readjusted. */
	cErr = PR_RC_DoAeAfAwb(m_hCamera,ActivateFlag);
	if ( cErr != prOK )
	{
		goto Error;
	}

	return;

Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnBrowse() 
{
	// TODO: Add your control notification handler code here
	LPMALLOC		pMalloc;
	BROWSEINFO		bi;
	char			szPath[MAX_PATH];
	LPITEMIDLIST	pidl;
	
	if( SHGetMalloc( &pMalloc ) == NOERROR )
	{
		/* Data is set to a BROWSEINFO structure object. */
		bi.hwndOwner = GetSafeHwnd();
		bi.pidlRoot = NULL;
		bi.pszDisplayName = szPath;
		bi.lpszTitle = _T("Select a Save Directory");
		bi.ulFlags = BIF_RETURNFSANCESTORS | BIF_RETURNONLYFSDIRS;
		bi.lpfn = NULL;
		bi.lParam = 0;
		/* A dialog is displayed. */
		if( (pidl=::SHBrowseForFolder(&bi)) != NULL )
		{
			if( ::SHGetPathFromIDList(pidl, szPath) )
			{
				/* A preservation path is set up. */
				m_CSavePath.SetWindowText( szPath );
			}
			pMalloc->Free(pidl);
		}	
		pMalloc->Release();
	}
}

LRESULT CRelCtrlDlg::WindowProc(UINT message, WPARAM wParam, LPARAM lParam) 
{
	// TODO: Add your specialized code here and/or call the base class
	prUInt8*	pBuffer = NULL;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		return WindowProc_CDSDK(message,wParam,lParam);
	}

	if ( message == m_ReleaseOnMessage )
	{
		/* if it is not shooting, */
		if (	m_fGetRelease == FALSE )
		{
			OnRelease();
		}
		return TRUE;
	}
	else if ( message == m_CameraShutDown )
	{
		OnDisconnect();
		return TRUE;
	}
	else if ( message == m_AbortPCEVF )
	{
		/* Is ViewFinder runnning? */
		if ( m_fVFEnd == TRUE )
		{
			/* Terminate ViewFinder */
			OnViewfinder();
		}
		return TRUE;
	}
	else if ( message == m_ChangeByUI )
	{
		if ( m_fInReleaseControl == TRUE )
		{
			UpdateSetting();
		}
		return TRUE;
	}
	return CDialog::WindowProc(message, wParam, lParam);
}

void CRelCtrlDlg::OnDestroy() 
{
	CDialog::OnDestroy();

	// TODO: Add your specialized code here and/or call the base class
	cdError		err;
	prResponse	cErr;
	char		szErrStr[256];

	/* End processing of CDSDK is performed. */
	err = CDFinishSDK();
	if ( GETERRORID(err) != cdOK )
	{
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}

	/* End processing of PS-ReC SDK is performed. */
	cErr = PR_FinishSDK();
	if ( cErr != prOK )
	{
		wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
		MessageBox( szErrStr );
	}
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CRelCtrlDlg::SetShootingModeBox()
{
	prUInt8*			pDataBuffer = NULL;
	prUInt8*			pBuffer = NULL;
	prUInt8				bFormFlag,bGetSet;
	prUInt8				bMode;
	prUInt16			index;
	prUInt16			wDataType;
	prUInt16			wNum,i;
	prUInt32			BufferSize = 0L;
	prResponse			err = prOK;
	prptpDevicePropCode	DevicePropCode;

	if ( !m_CShootingMode.IsWindowVisible() )
	{
		return;
	}
	m_CShootingMode.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CShootingMode.ResetContent();

	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];
	memset(pDataBuffer,0,MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	err = PR_GetDevicePropDesc(	m_hCamera,
								prPTP_DEV_PROP_EXPOSURE_MODE,
								&BufferSize,
								pDataBuffer );
	if ( err!= prOK )
	{
		/* Error, if not prPTP_DEV_PROP_EXPOSURE_MODE */
		goto SDK_Error;
	}

	/* Analysis of DevicePropDesc */
	pBuffer = pDataBuffer;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pBuffer);
	if ( DevicePropCode != prPTP_DEV_PROP_EXPOSURE_MODE )
	{
		/* Error, if not prPTP_DEV_PROP_EXPOSURE_MODE */
		goto SDK_Error;
	}
	pBuffer += sizeof(prptpDevicePropCode);

	/* DataType */
	wDataType = *((prUInt16 *)pBuffer);
	if ( wDataType != 0x0002 )
	{
		/* Error, if not prUInt8 */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pBuffer);
	if ( bGetSet != 0x01 )
	{
		/* Error, if not GetSet */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pBuffer +=  sizeof(prUInt8);

	/* CurrentValue */
	m_ShootingModeCntVal = *((prUInt8 *)pBuffer);
	pBuffer += sizeof(prUInt8);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pBuffer);
	if ( bFormFlag != 0x02 )
	{
		/* Error, if not Enumulation */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		bMode = *((prUInt8 *)pBuffer);
		switch ( bMode )
		{
			case 0x00:	index = m_CShootingMode.AddString( "Auto" );			break;
			case 0x01:	index = m_CShootingMode.AddString( "P" );				break;
			case 0x02:	index = m_CShootingMode.AddString( "Tv" );				break;
			case 0x03:	index = m_CShootingMode.AddString( "Av" );				break;
			case 0x04:	index = m_CShootingMode.AddString( "M" );				break;
			case 0x05:	index = m_CShootingMode.AddString( "A_DEP" );			break;
			case 0x06:	index = m_CShootingMode.AddString( "M_DEP" );			break;
			case 0x07:	index = m_CShootingMode.AddString( "Bulb" );			break;
			case 0x80:	index = m_CShootingMode.AddString( "CAMERAM" );			break;
			case 0x81:	index = m_CShootingMode.AddString( "MYCOLOR" );			break;
			case 0x82:	index = m_CShootingMode.AddString( "PORTRAIT" );		break;
			case 0x83:	index = m_CShootingMode.AddString( "LANDSCAPE" );		break;
			case 0x84:	index = m_CShootingMode.AddString( "NIGHTSCENE" );		break;
			case 0x85:	index = m_CShootingMode.AddString( "FOREST" );			break;
			case 0x86:	index = m_CShootingMode.AddString( "SNOW" );			break;
			case 0x87:	index = m_CShootingMode.AddString( "BEACH" );			break;
			case 0x88:	index = m_CShootingMode.AddString( "FIREWORKS" );		break;
			case 0x89:	index = m_CShootingMode.AddString( "PARTY" );			break;
			case 0x8a:	index = m_CShootingMode.AddString( "NIGHTSNAP" );		break;
			case 0x8b:	index = m_CShootingMode.AddString( "STITCH" );			break;
			case 0x8c:	index = m_CShootingMode.AddString( "MOVIE" );			break;
			case 0x8d:	index = m_CShootingMode.AddString( "CUSTOM" );			break;
			case 0x8e:	index = m_CShootingMode.AddString( "INTERVAL" );		break;
			case 0x8f:	index = m_CShootingMode.AddString( "DIGITALMACRO" );	break;
			case 0x90:	index = m_CShootingMode.AddString( "LONGSHUTTER" );		break;
			case 0x91:	index = m_CShootingMode.AddString( "UNDERWATER" );		break;
			case 0x92:	index = m_CShootingMode.AddString( "KIDSANDPETS" );		break;
			case 0x93:	index = m_CShootingMode.AddString( "FASTSHUTTER" );		break;
			case 0x94:	index = m_CShootingMode.AddString( "SLOWSHUTTER" );		break;
			case 0x95:	index = m_CShootingMode.AddString( "CUSTOM1" );			break;
			case 0x96:	index = m_CShootingMode.AddString( "CUSTOM2" );			break;
			case 0x97:	index = m_CShootingMode.AddString( "NEUTRAL" );			break;
			case 0x98:	index = m_CShootingMode.AddString( "GRAY" );			break;
			case 0x99:	index = m_CShootingMode.AddString( "SEPIA" );			break;
			case 0x9A:	index = m_CShootingMode.AddString( "VIVID" );			break;
			case 0x9B:	index = m_CShootingMode.AddString( "SPORTS" );			break;
			case 0x9C:	index = m_CShootingMode.AddString( "MACRO" );			break;
			case 0x9D:	index = m_CShootingMode.AddString( "SUPERMACRO" );		break;
			case 0x9E:	index = m_CShootingMode.AddString( "PANFOCUS" );		break;
			case 0x9F:	index = m_CShootingMode.AddString( "BW" );				break;
			case 0xA0:	index = m_CShootingMode.AddString( "FLASHINHIBIT" );	break;
			default:	index = m_CShootingMode.AddString( "(Error)" );			break;
		};
		m_CShootingMode.SetItemData( index, bMode );
		if ( m_ShootingModeCntVal == bMode )
		{
			// Set current setting to edit control of the Combo box 
			m_CShootingMode.SetCurSel( index );
		}
		pBuffer += sizeof(prUInt8);
	}
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}
	m_CShootingMode.EnableWindow( TRUE );

	return;

SDK_Error:
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( err != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}
}

void CRelCtrlDlg::SetExpoCompBox()
{
	prUInt8*			pDataBuffer = NULL;
	prUInt8*			pBuffer = NULL;
	prUInt8				bGetSet, bFormFlag;
	prUInt8				bMode;
	prUInt16			index;
	prUInt16			wDataType;
	prUInt16			wNum,i;
	prUInt32			BufferSize = 0L;
	prResponse			err = prOK;
	prptpDevicePropCode	DevicePropCode;

	if ( !m_CExpoComp.IsWindowVisible() )
	{
		return;
	}
	m_CExpoComp.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CExpoComp.ResetContent();
	
	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];
	memset(pDataBuffer,0,MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	err = PR_GetDevicePropDesc(	m_hCamera,
								prPTP_DEV_PROP_EXPOSURE_COMP,
								&BufferSize,
								pDataBuffer	);
	if ( err!= prOK )
	{
		goto SDK_Error;
	}

	/* Analysis of DevicePropDesc */
	pBuffer = pDataBuffer;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pBuffer);
	if ( DevicePropCode != prPTP_DEV_PROP_EXPOSURE_COMP )
	{
		/* Error, if not prPTP_DEV_PROP_EXPOSURE_COMP */
		goto SDK_Error;
	}
	pBuffer += sizeof(prptpDevicePropCode);

	/* DataType */
	wDataType = *((prUInt16 *)pBuffer);
	if ( wDataType != 0x0002 )
	{
		/* Error, if not prUInt8 */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pBuffer);
	if ( bGetSet != 0x01 )
	{
		/* Error, if not GetSet */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pBuffer +=  sizeof(prUInt8);

	/* CurrentValue */
	m_ExpoCompCntVal = *((prUInt8 *)pBuffer);
	pBuffer += sizeof(prUInt8);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pBuffer);
	if ( bFormFlag != 0x02 )
	{
		/* Error, if not Enumulation */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		bMode = *((prUInt8 *)pBuffer);
		/* add set data to the the combo box */
		switch ( bMode )
		{
			case 0x00:	index = m_CExpoComp.AddString( "+3" );		break;
			case 0x03:	index = m_CExpoComp.AddString( "+2(2/3)" );	break;
			case 0x04:	index = m_CExpoComp.AddString( "+2(1/2)" );	break;
			case 0x05:	index = m_CExpoComp.AddString( "+2(1/3)" );	break;
			case 0x08:	index = m_CExpoComp.AddString( "+2" );		break;
			case 0x0b:	index = m_CExpoComp.AddString( "+1(2/3)" );	break;
			case 0x0c:	index = m_CExpoComp.AddString( "+1(1/2)" );	break;
			case 0x0d:	index = m_CExpoComp.AddString( "+1(1/3)" );	break;
			case 0x10:	index = m_CExpoComp.AddString( "+1" );		break;
			case 0x13:	index = m_CExpoComp.AddString( "+2/3" );	break;
			case 0x14:	index = m_CExpoComp.AddString( "+1/2" );	break;
			case 0x15:	index = m_CExpoComp.AddString( "+1/3" );	break;
			case 0x18:	index = m_CExpoComp.AddString( "0" );		break;
			case 0x1b:	index = m_CExpoComp.AddString( "-1/3" );	break;
			case 0x1c:	index = m_CExpoComp.AddString( "-1/2" );	break;
			case 0x1d:	index = m_CExpoComp.AddString( "-2/3" );	break;
			case 0x20:	index = m_CExpoComp.AddString( "-1" );		break;
			case 0x23:	index = m_CExpoComp.AddString( "-1(1/3)" );	break;
			case 0x24:	index = m_CExpoComp.AddString( "-1(1/2)" );	break;
			case 0x25:	index = m_CExpoComp.AddString( "-1(2/3)" );	break;
			case 0x28:	index = m_CExpoComp.AddString( "-2" );		break;
			case 0x2b:	index = m_CExpoComp.AddString( "-2(1/3)" );	break;
			case 0x2c:	index = m_CExpoComp.AddString( "-2(1/2)" );	break;
			case 0x2d:	index = m_CExpoComp.AddString( "-2(2/3)" );	break;
			case 0x30:	index = m_CExpoComp.AddString( "-3" );		break;
			default:	index = m_CExpoComp.AddString( "(Error)" );	break;	
		};
		m_CExpoComp.SetItemData( index, bMode );
		if ( m_ExpoCompCntVal == bMode )
		{
			// Set current setting to edit control of the Combo box 
			m_CExpoComp.SetCurSel( index );
		}
		pBuffer += sizeof(prUInt8);
	}
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}
	m_CExpoComp.EnableWindow(TRUE);

	return;

SDK_Error:
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( err != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}
}

void CRelCtrlDlg::SetFlashBox()
{
	prUInt8*			pDataBuffer = NULL;
	prUInt8*			pBuffer = NULL;
	prUInt8				bGetSet, bFormFlag;
	prUInt8				bMode;
	prUInt16			index;
	prUInt16			wDataType;
	prUInt16			wNum,i;
	prUInt32			BufferSize = 0L;
	prResponse			err = prOK;
	prptpDevicePropCode	DevicePropCode;

	if ( !m_CFlash.IsWindowVisible() )
	{
		return;
	}
	m_CFlash.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CFlash.ResetContent();
	
	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];
	memset(pDataBuffer,0,MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	err = PR_GetDevicePropDesc(	m_hCamera,
								prPTP_DEV_PROP_STROBE_SETTING,
								&BufferSize,
								pDataBuffer	);
	if ( err != prOK )
	{
		goto SDK_Error;
	}

	/* Analysis of DevicePropDesc */
	pBuffer = pDataBuffer;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pBuffer);
	if ( DevicePropCode != prPTP_DEV_PROP_STROBE_SETTING )
	{
		/* Error, if not prPTP_DEV_PROP_STROBE_SETTING */
		goto SDK_Error;
	}
	pBuffer += sizeof(prptpDevicePropCode);

	/* DataType */
	wDataType = *((prUInt16 *)pBuffer);
	if ( wDataType != 0x0002 )
	{
		/* Error, if not prUInt8 */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pBuffer);
	if ( bGetSet != 0x01 )
	{
		/* Error, if not GetSet */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pBuffer +=  sizeof(prUInt8);

	/* CurrentValue */
	m_FlashCntVal = *((prUInt8 *)pBuffer);
	pBuffer += sizeof(prUInt8);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pBuffer);
	if ( bFormFlag != 0x02 )
	{
		/* Error, if not Enumulation-Form */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		bMode = *((prUInt8 *)pBuffer);
		/* add set data to the combo box */
		switch ( bMode )
		{
			case 0x00:	index = m_CFlash.AddString( "Off" );			break;
			case 0x01:	index = m_CFlash.AddString( "Auto" );			break;
			case 0x02:	index = m_CFlash.AddString( "On" );				break;
			case 0x03:	index = m_CFlash.AddString( "Red Eye" );		break;
			case 0x04:	index = m_CFlash.AddString( "Slow Sync." );		break;
			case 0x05:	index = m_CFlash.AddString( "Auto (Red Eye)" );	break;
			case 0x06:	index = m_CFlash.AddString( "On (Red Eye)" );	break;
			default:	index = m_CFlash.AddString( "(Error)" );		break;	
		};
		m_CFlash.SetItemData( index, bMode );
		if ( m_FlashCntVal == bMode )
		{
			// Set current setting to edit control of the Combo box 
			m_CFlash.SetCurSel( index );
		}
		pBuffer += sizeof(prUInt8);
	}
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}
	m_CFlash.EnableWindow(TRUE);

	return;

SDK_Error:
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( err != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}
}

void CRelCtrlDlg::SetISOSpeedBox()
{
	prUInt8*			pDataBuffer = NULL;
	prUInt8*			pBuffer = NULL;
	prUInt8				bGetSet, bFormFlag;
	prUInt16			index;
	prUInt16			wDataType;
	prUInt16			wNum,i;
	prUInt16			wMode;
	prUInt32			BufferSize = 0L;
	prResponse			err = prOK;
	prptpDevicePropCode	DevicePropCode;

	if ( !m_CISOSpeed.IsWindowVisible() )
	{
		return;
	}
	m_CISOSpeed.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CISOSpeed.ResetContent();

	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];
	memset(pDataBuffer,0,MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	err = PR_GetDevicePropDesc(	m_hCamera,
								prPTP_DEV_PROP_ISO,
								&BufferSize,
								pDataBuffer	);
	if ( err != prOK )
	{
		goto SDK_Error;
	}

	/* Analysis of DevicePropDesc */
	pBuffer = pDataBuffer;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pBuffer);
	if ( DevicePropCode != prPTP_DEV_PROP_ISO )
	{
		/* Error, if not prPTP_DEV_PROP_ISO */
		goto SDK_Error;
	}
	pBuffer += sizeof(prptpDevicePropCode);

	/* DataType */
	wDataType = *((prUInt16 *)pBuffer);
	if ( wDataType != 0x0004 )
	{
		/* Error, if not prUInt16 */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pBuffer);
	if ( bGetSet != 0x01 )
	{
		/* Error, if not GetSet */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pBuffer +=  sizeof(prUInt16);

	/* CurrentValue */
	m_ISOCntVal = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pBuffer);
	if ( bFormFlag != 0x02 )
	{
		/* Error, if not Enumulation-Form */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		wMode = *((prUInt16 *)pBuffer);
		/* add set data to the combo box */
		switch ( wMode )
		{
			case 0x0000:	index = m_CISOSpeed.AddString( "AUTO" );		break;
			case 0x0028:	index = m_CISOSpeed.AddString( "6" );			break;
			case 0x0030:	index = m_CISOSpeed.AddString( "12" );			break;
			case 0x0038:	index = m_CISOSpeed.AddString( "25" );			break;
			case 0x0040:	index = m_CISOSpeed.AddString( "50" );			break;
			case 0x0043:	index = m_CISOSpeed.AddString( "64" );			break;
			case 0x0045:	index = m_CISOSpeed.AddString( "80" );			break;
			case 0x0048:	index = m_CISOSpeed.AddString( "100" );			break;
			case 0x0050:	index = m_CISOSpeed.AddString( "200" );			break;
			case 0x0058:	index = m_CISOSpeed.AddString( "400" );			break;
			case 0x0060:	index = m_CISOSpeed.AddString( "800" );			break;
			case 0x0068:	index = m_CISOSpeed.AddString( "1600" );		break;
			case 0x0070:	index = m_CISOSpeed.AddString( "3200" );		break;
			case 0x0078:	index = m_CISOSpeed.AddString( "6400" );		break;
			default:		index = m_CISOSpeed.AddString( "(Error)" );		break;	
		};
		m_CISOSpeed.SetItemData( index, wMode );
		if ( m_ISOCntVal == wMode )
		{
			// Set current setting to edit control of the Combo box 
			m_CISOSpeed.SetCurSel( index );
		}
		pBuffer += sizeof(prUInt16);
	}
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}
	m_CISOSpeed.EnableWindow(TRUE);

	return;

SDK_Error:
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( err != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}
}

void CRelCtrlDlg::SetPhotoEffectBox()
{
	prUInt8*			pDataBuffer = NULL;
	prUInt8*			pBuffer = NULL;
	prUInt8				bGetSet, bFormFlag;
	prUInt16			index;
	prUInt16			wDataType;
	prUInt16			wNum,i;
	prUInt16			wMode;
	prUInt32			BufferSize = 0L;
	prResponse			err = prOK;
	prptpDevicePropCode	DevicePropCode;

	if ( !m_CPhotoEffect.IsWindowVisible() )
	{
		return;
	}
	m_CPhotoEffect.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CPhotoEffect.ResetContent();

	pDataBuffer = new prUInt8 [MY_BUFFER_SIZE];
	memset(pDataBuffer,0,MY_BUFFER_SIZE);
	BufferSize = MY_BUFFER_SIZE;
	err = PR_GetDevicePropDesc(	m_hCamera,
								prPTP_DEV_PROP_PHOTO_EFFECT,
								&BufferSize,
								pDataBuffer );
	if ( err != prOK )
	{
		goto SDK_Error;
	}

	/* Analysis of DevicePropDesc */
	pBuffer = pDataBuffer;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pBuffer);
	if ( DevicePropCode != prPTP_DEV_PROP_PHOTO_EFFECT )
	{
		/* Error, if not prPTP_DEV_PROP_PHOTO_EFFECT */
		goto SDK_Error;
	}
	pBuffer += sizeof( prptpDevicePropCode );

	/* DataType */
	wDataType = *((prUInt16 *)pBuffer);
	if ( wDataType != 0x0004 )
	{
		/* Error, if not prUInt16 */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pBuffer);
	if ( bGetSet != 0x01 )
	{
		/* Error, if not GetSet */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pBuffer +=  sizeof(prUInt16);

	/* CurrentValue */
	m_PhotEffectCntVal = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pBuffer);
	if ( bFormFlag != 0x02 )
	{
		/* Error, if not Enumulation-Form */
		goto SDK_Error;
	}
	pBuffer += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pBuffer);
	pBuffer += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		wMode = *((prUInt16 *)pBuffer);
		/* add set data to the combo box */
		switch ( wMode )
		{
			case 0x0000:	index = m_CPhotoEffect.AddString( "Off" );				break;
			case 0x0001:	index = m_CPhotoEffect.AddString( "Vivid" );			break;
			case 0x0002:	index = m_CPhotoEffect.AddString( "Neutral" );			break;
			case 0x0003:	index = m_CPhotoEffect.AddString( "Low Sharpening" );	break;
			case 0x0004:	index = m_CPhotoEffect.AddString( "Sepia" );			break;
			case 0x0005:	index = m_CPhotoEffect.AddString( "BW" );				break;
			default:		index = m_CPhotoEffect.AddString( "(Error)" );			break;	
		};
		m_CPhotoEffect.SetItemData( index, wMode );
		if ( m_PhotEffectCntVal == wMode )
		{
			// Set current setting to edit control of  the Combo box 
			m_CPhotoEffect.SetCurSel( index );
		}
		pBuffer += sizeof(prUInt16);
	}
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
		pDataBuffer = NULL;
	}
	m_CPhotoEffect.EnableWindow(TRUE);

	return;

SDK_Error:
	if ( pDataBuffer )
	{
		delete [] pDataBuffer;
	}

	if ( err != prOK )
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CRelCtrlDlg::OnSelchangeShootingMode() 
{
	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnSelchangeShootingMode_CDSDK();
		return;
	}

	prUInt8		wMode;
	prResponse	cErr = prOK;

	wMode = (prUInt8)m_CShootingMode.GetItemData( m_CShootingMode.GetCurSel() );
	if ( m_ShootingModeCntVal== wMode )
	{
		return;
	}

	/* Set values of the Device Properties */
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_EXPOSURE_MODE,
									(prUInt32)sizeof(prUInt8),
									&wMode );
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	m_ShootingModeCntVal = wMode;

	/* The process for updating the Device Properties */
	UpdateSetting();

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangeExpoComp() 
{
	prUInt8		wMode;
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnSelchangeExpoComp_CDSDK();
		return;
	}

	wMode = (prUInt8)m_CExpoComp.GetItemData( m_CExpoComp.GetCurSel() );
	if ( m_ExpoCompCntVal == wMode )
	{
		return;
	}

	/* Set values of the Device Properties */
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_EXPOSURE_COMP,
									(prUInt32)sizeof(prUInt8),
									&wMode	);
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	m_ExpoCompCntVal = wMode;

	/* The process for updating the Device Properties */
	UpdateSetting();

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangeFlash() 
{
	prUInt8		bMode;
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnSelchangeFlash_CDSDK();
		return;
	}

	bMode = (prUInt8)m_CFlash.GetItemData( m_CFlash.GetCurSel() );
	if (m_FlashCntVal == bMode)
	{
		return;
	}

	/* Set values of the Device Properties */
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_STROBE_SETTING,
									(prUInt32)sizeof(prUInt8),
									&bMode	);
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	m_FlashCntVal = bMode;

	/* The process for the Device Properties */
	UpdateSetting();

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangeISOSpeed() 
{
	prUInt16	wMode;
	prResponse	cErr = prOK;

	/* Is CD-SDK connection? */
	if ( m_hSource )
	{
		OnSelchangeISOSpeed_CDSDK();
		return;
	}

	wMode = (prUInt8)m_CISOSpeed.GetItemData( m_CISOSpeed.GetCurSel() );
	if ( m_ISOCntVal == wMode )
	{
		return;
	}

	/* Set values of the Device Properties */
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_ISO,
									(prUInt32)sizeof(prUInt16),
									&wMode	);
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	m_ISOCntVal = wMode;

	/* The process of updating the Device Properties */
	UpdateSetting();

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangePhotoEffect() 
{
	prUInt16	wMode;
	prResponse	cErr = prOK;

	/* Is CD-SDK Connection? */
	if ( m_hSource )
	{
		OnSelchangePhotoEffect_CDSDK();
		return;
	}

	wMode = (prUInt16)m_CPhotoEffect.GetItemData( m_CPhotoEffect.GetCurSel() );
	if ( m_PhotEffectCntVal == wMode )
	{
		return;
	}

	/* Set values of the Device Properties */
	cErr = PR_SetDevicePropValue(	m_hCamera,
									prPTP_DEV_PROP_PHOTO_EFFECT,
									(prUInt32)sizeof(prUInt16),
									&wMode	);
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	m_PhotEffectCntVal = wMode;

	/* The process for updating the Device Properies */
	UpdateSetting();

	return;

SDK_Error:
	char	szErrStr[256];

	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

prVoid CRelCtrlDlg::IsSupportedRCandEVF(
	prUInt8*	pDeviceInfo,	/* (IN) DeviceInfo data set*/
	prBoolean*	pbRC,
	prBoolean*	pbEVF,
	prBoolean*	pbAwb)
{
	prUInt8		*pDeviceInfoTmp;
	prUInt8		bNum;
	prUInt32	dwNum, i;
	prOperationCode	wOperation;
	
	/* Flag if the operation is support or not */
	prBoolean   fInitiate, fTerminate, fCapture;
	prBoolean	fFvInitiate,fFvTerminate,fAeAfAwb;

	fInitiate = FALSE;
	fTerminate = FALSE;
	fCapture = FALSE;
	fFvInitiate = FALSE;
	fFvTerminate = FALSE;
	fAeAfAwb = FALSE;

	/* Move the potiner to support operation */
	pDeviceInfoTmp = pDeviceInfo;

	/* Standard version */
	/* Vendor extendedID */
	/* Vendor extended version */
	pDeviceInfoTmp +=   sizeof(prUInt16)
					  + sizeof(prUInt32)
					  + sizeof(prUInt16);
	/* Vendor extended information */
	bNum = *((prUInt8 *)pDeviceInfoTmp);
	pDeviceInfoTmp +=   sizeof(prUInt8)
					  + sizeof(prWChar) * bNum;
	/* Function moded */
	pDeviceInfoTmp +=   sizeof(prUInt16);

	/* Suppored operations */
	dwNum = *((prUInt32 *)pDeviceInfoTmp);	/* number of elements */
	pDeviceInfoTmp +=   sizeof(prUInt32);

	/* Loop for the number of elements */
	for ( i = 0L; i < dwNum; i++ )
	{
		wOperation = *((prOperationCode *)pDeviceInfoTmp);
		switch ( wOperation )
		{
			case prPTP_INITIATE_RELEASE_CONTROL:
				fInitiate = TRUE;
				break;
			case prPTP_TERMINATE_RELEASE_CONTROL:
				fTerminate = TRUE;
				break;
			case prPTP_RC_CAPTURE:
				fCapture = TRUE;
				break;
			case prPTP_RC_INITIATE_VIEW_FINDER:
				fFvInitiate = TRUE;
				break;
			case prPTP_RC_TERMINATE_VIEW_FINDER:
				fFvTerminate = TRUE;
				break;
			case prPTP_RC_RELEASE_DO_AE_AF_AWB:
				fAeAfAwb = TRUE;
				break;
			default:
				break;
		}
		pDeviceInfoTmp += sizeof(prOperationCode);
	}
/* The following information is not checked */
	/* Supported events */
	/* Supported device properties */
	/* Supported captured image types */
	/* Supported image types */
	/* Company information */ 
	/* Model name */
	/* Device version */
	/* Serial number */

	/* Is Remote Capture supported? */
	if ( (fInitiate == TRUE) && (fTerminate == TRUE) && (fCapture == TRUE) )
	{
		*pbRC = TRUE;
	}
	/* Is Viewfinder supported? */
	if ( (fFvInitiate == TRUE) && (fFvTerminate == TRUE) )
	{
		*pbEVF = TRUE;
	}
	/* Is AeAfAwb supported? */
	if ( fAeAfAwb == TRUE )
	{
		*pbAwb = TRUE;
	}
}

prVoid	CRelCtrlDlg::IsSupportedCapRelPrm(
	prUInt8*	pDeviceInfo,	/* (IN) DeviceInfo data set */
	prBoolean*	pExpoureMode,
	prBoolean*	pExpoureComp,
	prBoolean*	pSTrobeSet,
	prBoolean*	pISO,
	prBoolean*	pPhotoEffect)
{
	prUInt8		*pDeviceInfoTmp;
	prUInt8		bNum;
	prUInt32	dwNum, i;
	prptpDevicePropCode	wDeviceProp;

	pDeviceInfoTmp = pDeviceInfo;

	/* Standard version */
	/* Vendor extended ID */
	/* Vendor extended version */
	pDeviceInfoTmp +=   sizeof(prUInt16)
					  + sizeof(prUInt32)
					  + sizeof(prUInt16);

	/* Vendor extended information */
	bNum = *((prUInt8 *)pDeviceInfoTmp);
	pDeviceInfoTmp +=   sizeof(prUInt8)
					  + sizeof(prWChar) * bNum;

	/* Function modes */
	pDeviceInfoTmp +=   sizeof(prUInt16);

	/* Supported operations */
	dwNum = *((prUInt32 *)pDeviceInfoTmp);
	pDeviceInfoTmp +=   sizeof(prUInt32)
					  + sizeof(prOperationCode) * dwNum;

	/* Supported events */
	dwNum = *((prUInt32 *)pDeviceInfoTmp);
	pDeviceInfoTmp +=   sizeof(prUInt32)
					  + sizeof(prUInt16) * dwNum;

	/* check supported device properties */
	dwNum = *((prUInt32 *)pDeviceInfoTmp);
	pDeviceInfoTmp +=   sizeof(prUInt32);
	for ( i = 0L; i < dwNum; i++ )
	{
		wDeviceProp = *((prptpDevicePropCode *)pDeviceInfoTmp);
		/* turn on the flags of the supported device properties */
		switch ( wDeviceProp )
		{
			case prPTP_DEV_PROP_EXPOSURE_MODE:
				*pExpoureMode = TRUE;
				break;
			case prPTP_DEV_PROP_EXPOSURE_COMP:
				*pExpoureComp = TRUE;
				break;
			case prPTP_DEV_PROP_STROBE_SETTING:
				*pSTrobeSet = TRUE;
				break;
			case prPTP_DEV_PROP_ISO:
				*pISO = TRUE;
				break;
			case prPTP_DEV_PROP_PHOTO_EFFECT:
				*pPhotoEffect = TRUE;
				break;
			default:
				break;
		}
		pDeviceInfoTmp += sizeof(prptpDevicePropCode);
	}
}

prBoolean	CRelCtrlDlg::IsSuppotedCapTransMode(
	prUInt8*	pDeviceInfo,
	prUInt16*	pwCurrentValue,
	prUInt16*	pwMode)
{
	prUInt8*	pDeviceInfoTmp;
	prUInt8		bFormFlag,bGetSet;
	prUInt16	wDataType = 0;
	prUInt16	wNum,i;
	prUInt16	wMode = 0;
	prptpDevicePropCode	DevicePropCode;

	pDeviceInfoTmp = pDeviceInfo;

	/* DevicePropCode */
	DevicePropCode = *((prptpDevicePropCode *)pDeviceInfoTmp);
	if ( DevicePropCode != prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE )
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prptpDevicePropCode);

	/* DataType */
	wDataType = *((prUInt16 *)pDeviceInfoTmp);
	if (wDataType != 0x0004)
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prUInt16);

	/* GetSet */
	bGetSet = *((prUInt8 *)pDeviceInfoTmp);
	if ( bGetSet != 0x01 )
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pDeviceInfoTmp +=  sizeof(prUInt16);

	/* CurrentValue */
	*pwCurrentValue = *((prUInt16 *)pDeviceInfoTmp);
	pDeviceInfoTmp += sizeof(prUInt16);

	/* FormFlag */
	bFormFlag = *((prUInt8 *)pDeviceInfoTmp);
	if ( bFormFlag != 0x02 )
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prUInt8);

	/* Enumeration-Form */
	wNum = *((prUInt16 *)pDeviceInfoTmp);
	pDeviceInfoTmp += sizeof(prUInt16);
	for ( i = 0; i < wNum; i++ )
	{
		wMode = *((prUInt16 *)pDeviceInfoTmp);
		/* check the supported transfer modes */
		switch ( wMode )
		{
			case 0x0001:
				*pwMode |= 0x0001;
				break;
			case 0x0002:
				*pwMode |= 0x0002;
				break;
			case 0x0004:
				*pwMode |= 0x0004;
				break;
			case 0x0008:
				*pwMode |= 0x0008;
				break;
			default:
				break;
		}
		pDeviceInfoTmp += sizeof(prUInt16);
	}

	return TRUE;
}

prBoolean	CRelCtrlDlg::IsReleasePictureType(prUInt8*	pDeviceInfo)
{
	prUInt8*			pDeviceInfoTmp;
	prUInt16			wDataType;
	prptpDevicePropCode	DevicePropCode;

	if ( !pDeviceInfo )
	{
		return FALSE;
	}

	pDeviceInfoTmp = pDeviceInfo;

	DevicePropCode = *((prptpDevicePropCode *)pDeviceInfoTmp);
	if (DevicePropCode != prPTP_DEV_PROP_FULLVIEW_FILE_FORMAT)
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prptpDevicePropCode);

	/* Data type */
	wDataType = *((prUInt16 *)pDeviceInfoTmp);
	if ( wDataType != 0x0002 )
	{
		return FALSE;
	}
	pDeviceInfoTmp += sizeof(prUInt16);

	/* Get/Set */
	pDeviceInfoTmp += sizeof(prUInt8);

	/* FactoryDefaultValue */
	pDeviceInfoTmp +=  sizeof(prUInt8);

	/* check the image type for current shooting */
	if ( *((prUInt8 *)pDeviceInfoTmp) == 0x01 )
	{
		m_FileType = prPTP_EXIF_JPEG;
	}
	else if ( *((prUInt8 *)pDeviceInfoTmp) == 0x02 )
	{
		m_FileType = prPTP_CRW;
	}
	else
	{
		return FALSE;
	}

	return TRUE;
}

prVoid	CRelCtrlDlg::UpdateSetting()
{
	prUInt8*	pBuffer = NULL;
	prUInt8*	pWorkBuffer = NULL;
	prUInt16	cwNum,cwCnt;
	prUInt32	clBufSize;
	prResponse	cErr = prOK;
	prptpDevicePropCode cGetDevCode;

	/* The list of the updated device propertis is acuired. */
	pBuffer = new prUInt8[MY_BUFFER_SIZE];
	memset( pBuffer, 0, MY_BUFFER_SIZE);
	clBufSize = MY_BUFFER_SIZE;
	cErr = PR_RC_GetChangedReleaseParamesList(	m_hCamera,
												&clBufSize,
												pBuffer	);
	if ( cErr != prOK )
	{
		goto SDK_Error;
	}

	/* If there are the updated device propertied, */
        /* the combo boxes about the updated device properties are updated. */
	pWorkBuffer = pBuffer;
	cwNum = *((prUInt16 *)pWorkBuffer);
	pWorkBuffer += sizeof(prUInt16);
	for ( cwCnt = 0;cwCnt < cwNum;cwCnt++ )
	{
		cGetDevCode = *((prptpDevicePropCode *)pWorkBuffer);
		switch ( cGetDevCode )
		{
			case prPTP_DEV_PROP_ISO:
				SetISOSpeedBox();
				break;
			case prPTP_DEV_PROP_PHOTO_EFFECT:
				SetPhotoEffectBox();
				break;
			case prPTP_DEV_PROP_EXPOSURE_MODE:
				SetShootingModeBox();
				GetCameraInformation();
				break;
			case prPTP_DEV_PROP_STROBE_SETTING:
				SetFlashBox();
				break;
			case prPTP_DEV_PROP_EXPOSURE_COMP:
				SetExpoCompBox();
				break;
		}
		pWorkBuffer += sizeof(prptpDevicePropCode);
	}

	if ( pBuffer )
	{
		delete [] pBuffer;
	}

	return;

SDK_Error:
	if ( pBuffer )
	{
		delete [] pBuffer;
	}

	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", cErr );
	MessageBox( szErrStr );
}

prVoid	CRelCtrlDlg::CameraDisconnect()
{

	if ( m_hCamera != 0L )
	{
		/* Is Viewfinder running? */
		if ( m_fVFEnd == TRUE )
		{
			/* A view finder is ended. */
			OnViewfinder();
		}
		/* Terminate Release Control mode */
		(prVoid)PR_TerminateReleaseControl(m_hCamera);
		/* Disconnect the camera */
		(prVoid)PR_DisconnectCamera(m_hCamera);
		/* Release the camera events */
		(prVoid)PR_ClearEventCallBack(m_hCamera);
		/* Destory camera object */
		(prVoid)PR_DestroyCameraObject(m_hCamera);

		m_hCamera = NULL;
		m_fInReleaseControl = FALSE;
	}
}
