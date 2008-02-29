// RelCtrlDlg.cpp : implementation file
//

#include "stdafx.h"
#include <vfw.h>

#include "RelCtrl.h"
#include "Progress.h"
#include "RelCtrlDlg.h"
#include "ComSettingDlg.h"

#include "cdAPI.h"

/////////////////////////////////////////////////////////////////////////////
// Call back function

/*	CDSDK	*/
/*	Camera Event Call Back	*/
cdUInt32 cdSTDCALL	CRelCtrlDlg::CamCallBackFunc_CDSDK(	cdEventID		EventID,
														const cdVoid*	pData,
														cdUInt32		DataSize,
														cdContext		Context )
{
	WPARAM			wParam;
	CRelCtrlDlg		*CpThis;
		
	CpThis = (CRelCtrlDlg*)Context;
	
	/* A security level is checked. */
	switch( cdEVENT_SEVERITY(EventID) )
	{
		case cdEVENT_SEVERITY_SHUTDOWN:
			wParam = ((WPARAM)BN_CLICKED<<16)|(0xFFFF&IDC_DISCONNECT);
			CpThis->PostMessage( WM_COMMAND, wParam, NULL );
			break;
		case cdEVENT_SEVERITY_NONE:
		case cdEVENT_SEVERITY_WARNING:
			break;
	}
	
	return	cdOK;
}

/*	CDSDK	*/
/*	Release Control CallBack	*/
cdUInt32 cdSTDCALL	CRelCtrlDlg::RelCallBackFunc_CDSDK(	cdReleaseEventID	EventID,
														const void *		pData,
														cdUInt32 			DataSize,
														cdContext			Context )
{
	cdError			err=cdOK;
	CRelCtrlDlg		*CpThis;
	cdUInt32		*dwpCount;
	CProgress		CProg;
	
	CpThis = (CRelCtrlDlg*)Context;
	
	switch( EventID )
	{
		case cdRELEASE_EVENT_RESET_HW_ERROR:
			break;
		case cdRELEASE_EVENT_CHANGED_BY_UI:
			CpThis->PostMessage( CpThis->m_ChangeByUI );
			break;
		case cdRELEASE_EVENT_RELEASE_START:
			break;
		case cdRESEASE_EVENT_RELEASE_COMPLETE:
			dwpCount = (cdUInt32*)pData;
			CpThis->PostMessage( CpThis->m_ReleaseCompleteMessage, (WPARAM)*dwpCount );
			break;
		case cdRELEASE_EVENT_CAM_RELEASE_ON:
			CpThis->PostMessage( CpThis->m_ReleaseOnMessage );
			break;
		case cdRELEASE_EVENT_ABORT_PC_EVF:
			CpThis->PostMessage( CpThis->m_AbortPCEVF );
			break;
	}
	
	return	err;
}

/*	CDSDK	*/
/* The function which receives the picture from a camera */
cdUInt32 cdSTDCALL	CRelCtrlDlg::ViewFinderCallBackFun_CDSDK(	cdVoid		*pBuf,
											 					cdUInt32	Size,
																cdUInt32	Format,
																cdContext	Context )
{
	cdError				err=cdOK;
	LPBYTE				bpPixel;
	PBITMAPFILEHEADER	pBmpFileheader;
	PBITMAPINFOHEADER	pBmpInfoheader;
	CRelCtrlDlg			*CpThis;
	
	CpThis = (CRelCtrlDlg*)Context;
	if( Format == FILEFORMAT_BMP )
	{
		pBmpFileheader = (PBITMAPFILEHEADER)pBuf;
		pBmpInfoheader = (PBITMAPINFOHEADER)((LPBYTE)pBuf + sizeof(BITMAPFILEHEADER));
		bpPixel = (LPBYTE)pBuf + pBmpFileheader->bfOffBits;
		
		/* A picture is saved at a buffer. */
		CpThis->m_CVFMutex.Lock();
		memcpy( CpThis->m_BackSurface.vpBits, bpPixel, pBmpInfoheader->biSizeImage );
		CpThis->m_CVFMutex.Unlock();
		/* A picture is displayed. */
		CpThis->m_CpVFThread_CDSDK->ResumeThread();

	}
	
	return	err;
}

/////////////////////////////////////////////////////////////////////////////
// Thread function

/*	CDSDK	*/
/* The thread for displaying a picture */
UINT WINAPI	CRelCtrlDlg::ViewFinderProc_CDSDK( LPVOID	vpParam )
{
	CRelCtrlDlg		*CpThis;
	BOOL			*fpEnd;
	HDC				hdcDest,hdcSrc;
		
	CpThis = (CRelCtrlDlg*)vpParam;
	fpEnd = (BOOL*)(&CpThis->m_fVFEnd);
	while( *fpEnd )
	{
		/* It waits until it can use data. */
		CpThis->m_CVFMutex.Lock();
		
		/* A picture is displayed. */
		hdcDest = ::GetDC( CpThis->m_CpViewFinder->GetSafeHwnd() );
		hdcSrc = ::CreateCompatibleDC( hdcDest );
		::SelectObject( hdcSrc, CpThis->m_BackSurface.hBmp );
		::BitBlt(	hdcDest,
					0,0,VIEWFINDER_WIDTH,VIEWFINDER_HEIGHT,
					hdcSrc,
					0,0,
					SRCCOPY );
		::DeleteDC( hdcSrc );
		::ReleaseDC( CpThis->m_CpViewFinder->GetSafeHwnd(), hdcDest );
		
		/* The ownership of data is released. */
		CpThis->m_CVFMutex.Unlock();
		/* A thread is suspended. */
		CpThis->m_CpVFThread_CDSDK->SuspendThread();
	}
	
	return	0;
}

/////////////////////////////////////////////////////////////////////////////
// CRelCtrlDlg dialog

void	CRelCtrlDlg::GetCameraInformation_CDSDK()
{
	cdError			err;
	CString			CAddStr;
	
	UpdateData( TRUE );
	m_CInfoString = "";
	
	/* Quality of image, image size, and the average size of a picture file are acquired. */
	cdCompQuality	Quality;
	cdImageSize		Size;
	err = CDGetImageFormatAttribute( m_hSource, &Quality, &Size );
	if( GETERRORID(err) == cdOK )
	{
		switch( Quality&0x00ff )
		{
			case cdCOMP_QUALITY_ECONOMY:
				CAddStr.Format( "ImageQuality = Economy\r\n" );
				break;
			case cdCOMP_QUALITY_NORMAL:
				CAddStr.Format( "ImageQuality = Normal\r\n" );
				break;
			case cdCOMP_QUALITY_FINE:
				CAddStr.Format( "ImageQuality = Fine\r\n" );
				break;
			case cdCOMP_QUALITY_RAW:
				CAddStr.Format( "ImageQuality = Raw\r\n" );
				break;
			case cdCOMP_QUALITY_SUPERFINE:
				CAddStr.Format( "ImageQuality = SuperFine\r\n" );
				break;
			case cdCOMP_QUALITY_UNKNOWN:
			default:
				CAddStr.Format( "ImageQuality = Unknown\r\n" );
				break;
		}
		m_CInfoString += CAddStr;
		
		switch( Size&0x00ff )
		{
			case cdIMAGE_SIZE_LARGE:
				CAddStr.Format( "ImageSize = Large\r\n" );
				break;
			case cdIMAGE_SIZE_MEDIUM:
				CAddStr.Format( "ImageSize = Medium\r\n" );
				break;
			case cdIMAGE_SIZE_SMALL:
				CAddStr.Format( "ImageSize = Small\r\n" );
				break;
			case cdIMAGE_SIZE_MEDIUM1:
				CAddStr.Format( "ImageSize = Medium1\r\n" );
				break;
			case cdIMAGE_SIZE_MEDIUM2:
				CAddStr.Format( "ImageSize = Medium2\r\n" );
				break;
			case cdIMAGE_SIZE_UNKNOWN:
			default:
				CAddStr.Format( "ImageSize = Unknown\r\n" );
				break;
		}
		m_CInfoString += CAddStr;
	}
	else if( GETERRORID(err) != cdNOT_SUPPORTED )
	{
		CAddStr.Format( "<err>CDGetImageFormatAttribute = 0x%08X\r\n", err );
		m_CInfoString += CAddStr;
	}
	
	/* Shooting mode is acquired. */
	cdShootingMode		ShootingMode;
	err = CDGetShootingMode( m_hSource, &ShootingMode );
	if( GETERRORID(err) == cdOK )
	{
		switch( ShootingMode )
		{
			case cdSHOOTING_MODE_AUTO:
				CAddStr.Format( "ShootingMode = Auto\r\n" );
				break;
			case cdSHOOTING_MODE_PROGRAM:
				CAddStr.Format( "ShootingMode = Program\r\n" );
				break;
			case cdSHOOTING_MODE_TV:
				CAddStr.Format( "ShootingMode = Tv\r\n" );
				break;
			case cdSHOOTING_MODE_AV:
				CAddStr.Format( "ShootingMode = Av\r\n" );
				break;
			case cdSHOOTING_MODE_MANUAL:
				CAddStr.Format( "ShootingMode = Manual\r\n" );
				break;
			case cdSHOOTING_MODE_A_DEP:
				CAddStr.Format( "ShootingMode = A Dep\r\n" );
				break;
			case cdSHOOTING_MODE_M_DEP:
				CAddStr.Format( "ShootingMode = M Dep\r\n" );
				break;
			case cdSHOOTING_MODE_BULB:
				CAddStr.Format( "ShootingMode = Bulb\r\n" );
				break;
			case cdSHOOTING_MODE_MANUAL_2:
				CAddStr.Format( "ShootingMode = Manual2\r\n" );
				break;
			case cdSHOOTING_MODE_FAR_SCENE:
				CAddStr.Format( "ShootingMode = Far Scene\r\n" );
				break;
			case cdSHOOTING_MODE_FAST_SHUTTER:
				CAddStr.Format( "ShootingMode = Fast Shutter\r\n" );
				break;
			case cdSHOOTING_MODE_SLOW_SHUTTER:
				CAddStr.Format( "ShootingMode = Slow Shutter\r\n" );
				break;
			case cdSHOOTING_MODE_NIGHT_SCENE:
				CAddStr.Format( "ShootingMode = Night Scene\r\n" );
				break;
			case cdSHOOTING_MODE_GRAY_SCALE:
				CAddStr.Format( "ShootingMode = Gray Scale\r\n" );
				break;
			case cdSHOOTING_MODE_SEPIA:
				CAddStr.Format( "ShootingMode = Mode Sepia\r\n" );
				break;
			case cdSHOOTING_MODE_PORTRAIT:
				CAddStr.Format( "ShootingMode = Portrait\r\n" );
				break;
			case cdSHOOTING_MODE_SPOT:
				CAddStr.Format( "ShootingMode = Spot\r\n" );
				break;
			case cdSHOOTING_MODE_MACRO:
				CAddStr.Format( "ShootingMode = Macro\r\n" );
				break;
			case cdSHOOTING_MODE_BW:
				CAddStr.Format( "ShootingMode = BW\r\n" );
				break;
			case cdSHOOTING_MODE_PANFOCUS:
				CAddStr.Format( "ShootingMode = Panfocus\r\n" );
				break;
			case cdSHOOTING_MODE_VIVID:
				CAddStr.Format( "ShootingMode = Vivid\r\n" );
				break;
			case cdSHOOTING_MODE_NEUTRAL:
				CAddStr.Format( "ShootingMode = Neutral\r\n" );
				break;
			case cdSHOOTING_MODE_INVALID:
			default:
				CAddStr.Format( "ShootingMode = Invalid\r\n" );
				break;
		}
		m_CInfoString += CAddStr;
	}
	else if( GETERRORID(err) != cdNOT_SUPPORTED )
	{
		CAddStr.Format( "<err>CDGetShootingMode = 0x%08X\r\n", err );
		m_CInfoString += CAddStr;
	}
	
	/* The number of sheets which can be remaining photoed is acquired. */
	cdUInt32	Num;
	err = CDGetNumAvailableShot( m_hSource, &Num );
	if( GETERRORID(err) == cdOK )
	{
		CAddStr.Format( "NumAvailableShot = %d\r\n", Num );
		m_CInfoString += CAddStr;
	}
	else if( GETERRORID(err) != cdNOT_SUPPORTED )
	{
		CAddStr.Format( "<err>CDGetNumAvailableShot = 0x%08X\r\n", err );
		m_CInfoString += CAddStr;
	}
	
	UpdateData( FALSE );
}

/* The state of a Release button is set up. */
cdError	CRelCtrlDlg::SetReleaseState_CDSDK()
{
	cdError			err=cdOK;
	int				i;
	long			lStyle;
	cdRelDataKind	Kind;
	CButton			*CpButtonList[3] = {	&m_CRelChk2,
											&m_CRelChk3,
											&m_CRelChk4 };
	cdRelDataKind	KindList[3] = {	cdREL_KIND_PICT_TO_PC,
									cdREL_KIND_THUMB_TO_CAM,
									cdREL_KIND_PICT_TO_CAM };
	

	if( m_fCamTypeEOS && !m_CRelChk2.GetCheck() )
	{
		m_CRelChk3.EnableWindow( FALSE );
		m_CRelChk4.EnableWindow( FALSE );
		m_CRelChk3.SetCheck( 0 );
		m_CRelChk4.SetCheck( 0 );
	}
	else
	{
		m_CRelChk3.EnableWindow( m_fMCard );
		m_CRelChk4.EnableWindow( m_fMCard );
	}

	/* The state of a release button is set up. */
	lStyle = GetWindowLong( m_CDisconnect.m_hWnd, GWL_STYLE );
	if( (m_CRelChk4.GetCheck() || m_CRelChk3.GetCheck() || m_CRelChk2.GetCheck()) 
		&& !(lStyle&WS_DISABLED) )
	{
		m_CRelease.EnableWindow( TRUE );
	}
	else
	{
		m_CRelease.EnableWindow( FALSE );
	}
	
	if( m_hSource )
	{
		/* The Kind of Release is acquired. */
		Kind = 0;
		for( i=0 ; i<4 ; i++ )
		{
			if( CpButtonList[i]->GetCheck() )
			{
				Kind |= KindList[i];
			}
		}
		
		if( Kind )
		{
			/* The Kind of Release is set up. */
			err = CDSelectReleaseDataKind( m_hSource, Kind );
			if( GETERRORID(err) != cdOK )
			{
				return	err;
			}
		}
	}
	
	return	err;
}

void CRelCtrlDlg::OnConnect_CDSDK(cdSourceInfo* pSSrcInfo)
{
	cdError			err;

	/* A device is opened. */
	err = CDOpenSource( pSSrcInfo, &m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* UI is locked so that information may not be changed. */
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The function which receives the event from a camera is set up. */
	err = CDRegisterEventCallbackFunction(	m_hSource,
											CamCallBackFunc_CDSDK,
											(cdContext)this,
											&m_hCallbackFunction );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The existence of memory card is checked. */
	cdHEnum			hEnumVol;
	cdHVolume		hVolume;
	cdVolumeInfo	SVolInfo;
	
	m_fMCard = FALSE;
	/*Volumes are enumerated.*/
	err = CDEnumVolumeReset( m_hSource, &hEnumVol );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* It repeats until it enumerates all volumes. */
	while( (err = CDEnumVolumeNext( hEnumVol, &hVolume )) == cdOK )
	{
		/* The information on volume is acquired. */
		err = CDGetVolumeInfo( hVolume, &SVolInfo );
		if( GETERRORID(err) != cdOK )
		{
			CDEnumVolumeRelease( hEnumVol );
			goto	camerr;
		}
		
		if( SVolInfo.TotalSpace )
		{
			m_fMCard = TRUE;
			break;
		}
	}	
	if( GETERRORID(err) != cdOK && GETERRORID(err) != cdENUM_NA)
	{
		CDEnumVolumeRelease( hEnumVol );
		goto	camerr;
	}
	
	/* Listing of volume is ended. */
	err = CDEnumVolumeRelease( hEnumVol );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	cdHEnum		hEnum;
	cdUInt32	bufsize;
	cdChar		ModelName[32];
	err = CDEnumDevicePropertyReset(m_hSource, 0, &hEnum);
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}

	bufsize = sizeof(m_RelControlCap);
	err = CDGetDevicePropertyData(m_hSource, cdDEVICE_PROP_RELEASE_CONTROL_CAP, &bufsize, &m_RelControlCap, 0);
	if( GETERRORID(err) != cdOK )
	{
		CDEnumDevicePropertyRelease(hEnum);
		goto	camerr;
	}
	if( m_RelControlCap&cdRELEASE_CONTROL_CAP_SUPPORT==0 ){
		err = cdNOT_SUPPORTED;
		goto camerr;
	}

	bufsize = sizeof(ModelName);
	memset(ModelName, 0, bufsize);
	err = CDGetDevicePropertyData(m_hSource, cdDEVICE_PROP_MODEL_NAME, &bufsize, ModelName, 0);
	if( GETERRORID(err) != cdOK )
	{
		CDEnumDevicePropertyRelease(hEnum);
		goto	camerr;
	}
	if( strstr(ModelName, "EOS") )
	{
		m_fCamTypeEOS = TRUE;
	}
	else
	{
		m_fCamTypeEOS = FALSE;
	}

	err = CDEnumDevicePropertyRelease(hEnum);
	hEnum = NULL;
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}

	/* A camera is set as remote release control mode. */
	err = CDEnterReleaseControl(m_hSource,
								RelCallBackFunc_CDSDK,
								(cdContext)this );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	m_CConnect.EnableWindow( FALSE );
	m_CDisconnect.EnableWindow( TRUE );
	m_CRelChk2.EnableWindow( TRUE );

	if( m_RelControlCap&cdRELEASE_CONTROL_CAP_VIEWFINDER )
	{
		m_CViewFinder.EnableWindow( TRUE );
		m_CAEAF.EnableWindow( TRUE );
	}

	err = SetReleaseState_CDSDK();
	if( GETERRORID(err) != cdOK )
	{
		CDExitReleaseControl( m_hSource );
		goto	camerr;
	}
	
	ShowReleaseParam(pSSrcInfo->Name);
	/* ShoottingMode */
	SetShootingModeBox_CDSDK();
	/* ShoottingMode */
	SetExpoCompBox_CDSDK();
	/* Flash */
	SetFlashBox_CDSDK();
	/* ISO */
	SetISOSpeedBox_CDSDK();
	/* PhotoEffect */
	SetPhotoEffectBox_CDSDK();

	GetCameraInformation_CDSDK();
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		CDExitReleaseControl( m_hSource );
		goto	camerr;
	}
	
	return;
	
camerr:
	if(err != cdNOT_SUPPORTED ){
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}

	if( m_hSource )
	{
		if( m_hCallbackFunction )
		{
			CDUnregisterEventCallbackFunction( m_hSource, m_hCallbackFunction );
			m_hCallbackFunction = NULL;
		}
		
		CDUnlockUI( m_hSource );
		CDCloseSource( m_hSource );
		m_hSource = NULL;
	}
	

}
void CRelCtrlDlg::OnDisconnect_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError	err;
	
	if( m_fVFEnd )
	{
		/* A view finder is ended. */
		err = CDTermViewfinder( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* A thread is ended. */
		m_fVFEnd = FALSE;
		m_CpVFThread_CDSDK->ResumeThread();
		WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
		
		m_CViewFinder.SetWindowText( "Start" );
		Invalidate();
		UpdateWindow();	
	}
	
	if( m_hSource )
	{
		/* Remote release control mode is ended. */
		err = CDExitReleaseControl( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* The function which receives the event from a camera is canceled. */
		if( m_hCallbackFunction )
		{
			err = CDUnregisterEventCallbackFunction( m_hSource, m_hCallbackFunction );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			m_hCallbackFunction = NULL;
		}
		
		/* A device is closed. */
		err = CDCloseSource( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		m_hSource = NULL;
	}
	
	m_CConnect.EnableWindow( TRUE );
	m_CDisconnect.EnableWindow( FALSE );
	m_CRelease.EnableWindow( FALSE );
	m_CViewFinder.EnableWindow( FALSE );
	m_CAEAF.EnableWindow( FALSE );
	m_CRelChk2.EnableWindow( FALSE );
	m_CRelChk3.EnableWindow( FALSE );
	m_CRelChk4.EnableWindow( FALSE );

	ShowReleaseParam(NULL);

	UpdateData(TRUE);
	m_CInfoString = "";
	UpdateData(FALSE);

	return;
	
camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnEnd_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError			err;
	
	
	if( m_fVFEnd )
	{
		/* A view finder is ended. */
		err = CDTermViewfinder( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* A thread is ended. */
		m_fVFEnd = FALSE;
		m_CpVFThread_CDSDK->ResumeThread();
		WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
	}
	
	if( m_hSource )
	{
		/* Remote release control mode is ended. */
		err = CDExitReleaseControl( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		if( m_hCallbackFunction )
		{
			/* The function which receives the event from a camera is canceled. */
			err = CDUnregisterEventCallbackFunction( m_hSource, m_hCallbackFunction );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			m_hCallbackFunction = NULL;
		}
		
		/* A device is closed. */
		err = CDCloseSource( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		m_hSource = NULL;
	}
	
	/* A picture buffer is deleted. */
	if( m_BackSurface.hBmp )
	{
		DeleteObject( m_BackSurface.hBmp );
		m_BackSurface.hBmp = NULL;
	}
	
	EndDialog( 0 );	
	return;
	
camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	
	if( m_fVFEnd )
	{
		CDTermViewfinder( m_hSource );
		m_fVFEnd = FALSE;
		m_CpVFThread_CDSDK->ResumeThread();
		WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
	}
	
	if( m_hSource )
	{
		CDExitReleaseControl( m_hSource );
		if( m_hCallbackFunction )
		{
			CDUnregisterEventCallbackFunction( m_hSource, m_hCallbackFunction );
			m_hCallbackFunction = NULL;
		}
		CDCloseSource( m_hSource );
		m_hSource = NULL;
	}
	
	if( m_BackSurface.hBmp )
	{
		DeleteObject( m_BackSurface.hBmp );
		m_BackSurface.hBmp = NULL;
	}
	
	EndDialog( 0 );	
}

void CRelCtrlDlg::OnRelease_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError		err;
	BOOL		fRes;
	CProgress	CProg;
	char		szSavePath[MAX_PATH];
	cdUInt32	NumData;
	
	/* UI is locked so that information may not be changed. */
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* It sets up so that a complete message may be disregarded. */
	m_fProgramRelease = TRUE;
	
	/* A special camera ends a view finder. */
	if( m_RelControlCap&cdRELEASE_CONTROL_CAP_ABORT_VIEWFINDER )
	{
		if( m_fVFEnd )
		{
			/* A view finder is ended. */
			err = CDTermViewfinder( m_hSource );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			
			/* A thread is ended. */
			m_fVFEnd = FALSE;
			m_CpVFThread_CDSDK->ResumeThread();
			WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
			
			m_CViewFinder.SetWindowText( "Start" );
			Invalidate();
			UpdateWindow();	
		}
	}
	
	/* A photograph is taken. */
	NumData = 0;
	err = CDRelease( m_hSource, FALSE, NULL, NULL, cdPROG_NO_REPORT, &NumData );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The directory to save is acquired. */
	GetSavePath( szSavePath, MAX_PATH );
	
	/* The photoed picture is saved. */
	fRes = CProg.GetReleaseData_CDSDK( m_hSource, NumData, szSavePath );
	if( !fRes )
	{
		goto	apierr;
	}
	else if( GETERRORID(CProg.m_LastErr_CDSDK) == cdOPERATION_CANCELLED )
	{
		m_fProgramRelease = FALSE;
		CDUnlockUI( m_hSource );
		return;
	}
	else if( GETERRORID(CProg.m_LastErr_CDSDK) != cdOK )
	{
		err = CProg.m_LastErr_CDSDK;
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* It sets up so that a complete message may be received. */
	m_fProgramRelease = FALSE;
	return;
	
camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	m_fProgramRelease = FALSE;
	return;
	
apierr:
	MessageBox( "API Error" );
	CDUnlockUI( m_hSource );
	m_fProgramRelease = FALSE;
	
}

void CRelCtrlDlg::OnRelcheck1_CDSDK() 
{
	// TODO: Add your control notification handler code here
	/* UI is locked so that information may not be changed. */
	cdError		err;
	
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	err = SetReleaseState();
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;

camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::OnRelcheck2_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError		err;
	
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	err = SetReleaseState();
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;

camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::OnRelcheck3_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError		err;
	
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	err = SetReleaseState();
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;

camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::OnRelcheck4_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError		err;
	
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	err = SetReleaseState();
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;

camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::OnViewfinder_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError			err;
	cdStgMedium		VFMedium;
	
	/* UI is locked so that information may not be changed. */
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	if( !m_fVFEnd )
	{
		/* The thread which displays view finder is created. */
		m_fVFEnd = TRUE;
		m_CpVFThread_CDSDK = AfxBeginThread((AFX_THREADPROC)ViewFinderProc_CDSDK,
											(LPVOID)this,
											0,
											0,
											CREATE_SUSPENDED,
											NULL );

		/* A view finder is started. */
		VFMedium.Type = cdMEMTYPE_STREAM;
		err = CDStartViewfinder(	m_hSource,
									FILEFORMAT_BMP,
									ViewFinderCallBackFun_CDSDK,
									(cdContext)this );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		m_CViewFinder.SetWindowText( "Stop" );
	}
	else
	{
		/* A view finder is ended. */
		err = CDTermViewfinder( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* A thread is ended. */
		m_fVFEnd = FALSE;
		m_CpVFThread_CDSDK->ResumeThread();
		WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
		
		m_CViewFinder.SetWindowText( "Start" );		
		Invalidate();
		UpdateWindow();	
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;

camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::OnAeaf_CDSDK() 
{
	// TODO: Add your control notification handler code here
	cdError		err;
	cdUInt32  	ActivateFlag = 0x00000007;
	char		szErrStr[256];
	
	/* UI is locked so that information may not be changed. */
	err = CDLockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* AE and AF are readjusted. */
	err = CDActViewfinderAutoFunctions( m_hSource, ActivateFlag );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	/* The lock of UI is canceled. */
	err = CDUnlockUI( m_hSource );
	if( GETERRORID(err) != cdOK )
	{
		goto	camerr;
	}
	
	return;
	
camerr:
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	
}

void CRelCtrlDlg::SetShootingModeBox_CDSDK()
{
	//
	// Update ShootingMode Combo Box
	//

	cdError			err = cdOK;
	cdShootingMode	CurrValue = cdSHOOTING_MODE_INVALID;
	cdShootingMode	PossibleValue = cdSHOOTING_MODE_INVALID;
	cdHEnum			hEnumPossibleValue = NULL;

	if( !m_CShootingMode.IsWindowVisible() )
	{
		return;
	}
	m_CShootingMode.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CShootingMode.ResetContent();

	// Get current setting for default value.
	err = CDGetShootingMode(m_hSource, &CurrValue);
	if(err) goto camerr;

	//
	// Get all shooting modes which can be set to camera,
	// and set the list box.
	err = CDEnumShootingModeReset(m_hSource, &hEnumPossibleValue);
	if(err) goto camerr;

	while( (err = CDEnumShootingModeNext(hEnumPossibleValue, &PossibleValue) ) == cdOK)
	{
		int index;
		switch(PossibleValue)
		{
			case cdSHOOTING_MODE_INVALID:		index = m_CShootingMode.AddString( "(Invalid)" );		break;
			case cdSHOOTING_MODE_AUTO:			index = m_CShootingMode.AddString( "Auto" );			break;
			case cdSHOOTING_MODE_PROGRAM:		index = m_CShootingMode.AddString( "Program" );			break;
			case cdSHOOTING_MODE_TV:			index = m_CShootingMode.AddString( "Tv" );				break;
			case cdSHOOTING_MODE_AV:			index = m_CShootingMode.AddString( "Av" );				break;
			case cdSHOOTING_MODE_MANUAL:		index = m_CShootingMode.AddString( "Manual" );			break;
			case cdSHOOTING_MODE_A_DEP:			index = m_CShootingMode.AddString( "A_DEP" );			break;
			case cdSHOOTING_MODE_M_DEP:			index = m_CShootingMode.AddString( "M_DEP" );			break;
			case cdSHOOTING_MODE_BULB:			index = m_CShootingMode.AddString( "Bulb" );			break;
			case cdSHOOTING_MODE_MANUAL_2:		index = m_CShootingMode.AddString( "Manual" );			break;
			case cdSHOOTING_MODE_FAR_SCENE:		index = m_CShootingMode.AddString( "Far Scene" );		break;
			case cdSHOOTING_MODE_FAST_SHUTTER:	index = m_CShootingMode.AddString( "Fast Shutter" );	break;
			case cdSHOOTING_MODE_SLOW_SHUTTER:	index = m_CShootingMode.AddString( "Slow Shutter" );	break;
			case cdSHOOTING_MODE_NIGHT_SCENE:	index = m_CShootingMode.AddString( "Night Scene" );		break;
			case cdSHOOTING_MODE_GRAY_SCALE:	index = m_CShootingMode.AddString( "Gray Scale" );		break;
			case cdSHOOTING_MODE_SEPIA:			index = m_CShootingMode.AddString( "Sepia" );			break;
			case cdSHOOTING_MODE_PORTRAIT:		index = m_CShootingMode.AddString( "Portrait" );		break;
			case cdSHOOTING_MODE_SPOT:			index = m_CShootingMode.AddString( "Spot" );			break;
			case cdSHOOTING_MODE_MACRO:			index = m_CShootingMode.AddString( "Macro" );			break;
			case cdSHOOTING_MODE_BW:			index = m_CShootingMode.AddString( "BW" );				break;
			case cdSHOOTING_MODE_PANFOCUS:		index = m_CShootingMode.AddString( "Panfocus" );		break;
			case cdSHOOTING_MODE_VIVID:			index = m_CShootingMode.AddString( "Vivid" );			break;
			case cdSHOOTING_MODE_NEUTRAL:		index = m_CShootingMode.AddString( "Neutral" );			break;
			default:							index = m_CShootingMode.AddString( "(Error)" );	
		};
		
		m_CShootingMode.SetItemData( index, PossibleValue );

		if(PossibleValue == CurrValue)
		{
			// Set current value to edit control of Combo box 
			m_CShootingMode.SetCurSel( index );
		}
	}

	err = CDEnumShootingModeRelease(hEnumPossibleValue);
	hEnumPossibleValue = NULL;
	if(err) goto camerr;

	m_CShootingMode.EnableWindow( TRUE );
		
	return;

camerr:
	if(hEnumPossibleValue)
	{
		CDEnumShootingModeRelease(hEnumPossibleValue);
	}

	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );

	return;
}

void CRelCtrlDlg::SetExpoCompBox_CDSDK()
{
	//
	// Update Exposure Compensation Combo Box
	//

	cdError			err = cdOK;
	cdCompensation 	CurrValue = cdCOMP_NA;
	cdCompensation 	PossibleValue = cdCOMP_NA;
	cdHEnum			hEnumPossibleValue = NULL;

	if( !m_CExpoComp.IsWindowVisible() )
	{
		return;
	}
	m_CExpoComp.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CExpoComp.ResetContent();

	// Get current setting for default value.
	err = CDGetExposureComp(m_hSource, &CurrValue);
	if(err) goto camerr;

	//
	// Get all exposure compensation values which can be set to camera,
	// and set the list box.
	err = CDEnumExposureCompReset(m_hSource, &hEnumPossibleValue);
	if(err) goto camerr; 

	while( (err = CDEnumExposureCompNext(hEnumPossibleValue, &PossibleValue) ) == cdOK)
	{
		int index;
		switch(PossibleValue)
		{
			case cdCOMP_300_PLUS:	index = m_CExpoComp.AddString( "+ 3"	);		break;
			case cdCOMP_266_PLUS:	index = m_CExpoComp.AddString( "+ 2 2/3");		break;
			case cdCOMP_250_PLUS:	index = m_CExpoComp.AddString( "+ 2 1/2");		break;
			case cdCOMP_233_PLUS:	index = m_CExpoComp.AddString( "+ 2 1/3");		break;
			case cdCOMP_200_PLUS:	index = m_CExpoComp.AddString( "+ 2"	);		break;
			case cdCOMP_166_PLUS:	index = m_CExpoComp.AddString( "+ 1 2/3");		break;
			case cdCOMP_150_PLUS:	index = m_CExpoComp.AddString( "+ 1 1/2");		break;
			case cdCOMP_133_PLUS:	index = m_CExpoComp.AddString( "+ 1 1/3");		break;
			case cdCOMP_100_PLUS:	index = m_CExpoComp.AddString( "+ 1"	);		break;
			case cdCOMP_066_PLUS:	index = m_CExpoComp.AddString( "+ 2/3"	);		break;
			case cdCOMP_050_PLUS:	index = m_CExpoComp.AddString( "+ 1/2"	);		break;
			case cdCOMP_033_PLUS:	index = m_CExpoComp.AddString( "+ 1/3"	);		break;
			case cdCOMP_000_PLUS:	index = m_CExpoComp.AddString( "0"		);		break;
			case cdCOMP_033_MINUS:	index = m_CExpoComp.AddString( "- 1/3"	);		break;
			case cdCOMP_050_MINUS:	index = m_CExpoComp.AddString( "- 1/2"	);		break;
			case cdCOMP_066_MINUS:	index = m_CExpoComp.AddString( "- 2/3"	);		break;
			case cdCOMP_100_MINUS:	index = m_CExpoComp.AddString( "- 1"	);		break;
			case cdCOMP_133_MINUS:	index = m_CExpoComp.AddString( "- 1 1/3");		break;
			case cdCOMP_150_MINUS:	index = m_CExpoComp.AddString( "- 1 1/2");		break;
			case cdCOMP_166_MINUS:	index = m_CExpoComp.AddString( "- 1 2/3");		break;
			case cdCOMP_200_MINUS:	index = m_CExpoComp.AddString( "- 2"	);		break;
			case cdCOMP_233_MINUS:	index = m_CExpoComp.AddString( "- 2 1/3");		break;
			case cdCOMP_250_MINUS:	index = m_CExpoComp.AddString( "- 2 1/2");		break;
			case cdCOMP_266_MINUS:	index = m_CExpoComp.AddString( "- 2 2/3");		break;
			case cdCOMP_300_MINUS:	index = m_CExpoComp.AddString( "- 3"	);		break;
			case cdCOMP_NA:			index = m_CExpoComp.AddString( "(NA)"	);		break;
			default:				index = m_CExpoComp.AddString( "(Error)" );	
		};
		
		m_CExpoComp.SetItemData( index, PossibleValue );

		if(PossibleValue == CurrValue)
		{
			// Set current value to edit control of Combo box 
			m_CExpoComp.SetCurSel( index );
		}
	}
	err = CDEnumExposureCompRelease(hEnumPossibleValue);
	hEnumPossibleValue = NULL;
	if(err) goto camerr;

	m_CExpoComp.EnableWindow( TRUE );
	
	return;

camerr:
	if(hEnumPossibleValue)
	{
		CDEnumExposureCompRelease(hEnumPossibleValue);
	}

	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );

	return;
}

void CRelCtrlDlg::SetFlashBox_CDSDK()
{
	//
	// Update Flash Combo Box
	//

	cdError			err = cdOK;
	cdFlashMode 	CurrValue = cdFLASH_MODE_NA;
	cdFlashMode 	PossibleValue = cdFLASH_MODE_NA;
	cdHEnum			hEnumPossibleValue = NULL;
	cdCompensation  Comp = cdCOMP_NA;

	if( !m_CFlash.IsWindowVisible() )
	{
		return;
	}
	m_CFlash.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CFlash.ResetContent();

	// Get current setting for default value.
	err = CDGetFlashSetting(m_hSource, &CurrValue, &Comp);
	if(err) goto camerr;

	//
	// Get all flash settings which can be set to camera,
	// and set the list box.
	err = CDEnumFlashSettingReset(m_hSource, &hEnumPossibleValue);
	err &= 0x000f;
	if(err) goto camerr;

	while( (err = CDEnumFlashSettingNext(hEnumPossibleValue, &PossibleValue) ) == cdOK)
	{
		int index;
		switch(PossibleValue)
		{
			case cdFLASH_MODE_OFF:					index = m_CFlash.AddString( "Off" );			break;
			case cdFLASH_MODE_AUTO:					index = m_CFlash.AddString( "Auto" );			break;
			case cdFLASH_MODE_ON:					index = m_CFlash.AddString( "On" );				break;
			case cdFLASH_MODE_RED_EYE:				index = m_CFlash.AddString( "Red Eye" );		break;
			case cdFLASH_MODE_SLOW_SYNC:			index = m_CFlash.AddString( "Slow Sync." );		break;
			case cdFLASH_MODE_AUTO_PLUS_RED_EYE:	index = m_CFlash.AddString( "Auto (Red Eye)" );	break;
			case cdFLASH_MODE_ON_PLUS_RED_EYE:		index = m_CFlash.AddString( "On (Red Eye)" );	break;
			case cdFLASH_MODE_NA:					index = m_CFlash.AddString( "(NA)" );			break;
			default:								index = m_CFlash.AddString( "(Error)" );	
		};
		
		m_CFlash.SetItemData( index, PossibleValue );

		if(PossibleValue == CurrValue)
		{
			// Set current setting to edit control of Combo box 
			m_CFlash.SetCurSel( index );
		}
	
	}

	err = CDEnumFlashSettingRelease(hEnumPossibleValue);
	hEnumPossibleValue = NULL;
	if(err) goto camerr;

	m_CFlash.EnableWindow( TRUE );

	return;

camerr:
	if(hEnumPossibleValue)
	{
		CDEnumFlashSettingRelease(hEnumPossibleValue);
	}

	if(err != cdNOT_SUPPORTED){
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );
	}

	return;
}


void CRelCtrlDlg::SetISOSpeedBox_CDSDK()
{
	//
	// Update ISO Combo Box
	//

	cdError					err = cdOK;

	cdRelCamSettingID		TargetSettingID = cdREL_SET_ISO_SPEED_RATINGS;

	cdUInt16				CurrValue = cdREL_VAL_ISO_NA;
	cdUInt16				PossibleValue = cdREL_VAL_ISO_NA;
	cdHEnum					hEnumCamSetting = NULL;
	cdHEnum					hEnumPossibleValue = NULL;
	cdUInt32				BufSize = 0;
	cdRelCamSettingStruct	relStruct;

	memset(&relStruct, 0, sizeof(relStruct));

	if( !m_CISOSpeed.IsWindowVisible() )
	{
		return;
	}
	m_CISOSpeed.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CISOSpeed.ResetContent();

	//
	// Check whether the camera supports the ISO setting. 
	err = CDEnumRelCamSettingReset( m_hSource, &hEnumCamSetting);
	if(err) goto camerr;

	while( (err = CDEnumRelCamSettingNext(hEnumCamSetting, &relStruct) )== cdOK)
	{
		if(relStruct.SettingID != TargetSettingID)
		{
			continue;
		}

		// Camera supports ISO setting. //

		BufSize = sizeof(CurrValue);

		// Get current setting for default value.
		err = CDGetRelCamSettingData(m_hSource, TargetSettingID, &BufSize, &CurrValue);
		if(err) goto camerr;
		
		//
		// Get all ISO values which can be set to camera,
		// and set the list box.
		err = CDEnumRelCamSettingDataReset(m_hSource, TargetSettingID, &hEnumPossibleValue, &BufSize);
		if(err) goto camerr;

		if(BufSize == sizeof(PossibleValue))
		{
			while( (err = CDEnumRelCamSettingDataNext(hEnumPossibleValue, sizeof(PossibleValue), &PossibleValue) ) == cdOK)
			{
				int index;
				switch(PossibleValue){
					case cdREL_VAL_ISO_AUTO:	index = m_CISOSpeed.AddString( "AUTO" );		break;
					case cdREL_VAL_ISO_6:		index = m_CISOSpeed.AddString( "6" );			break;
					case cdREL_VAL_ISO_8:		index = m_CISOSpeed.AddString( "8" );			break;
					case cdREL_VAL_ISO_10:		index = m_CISOSpeed.AddString( "10" );			break;
					case cdREL_VAL_ISO_12:		index = m_CISOSpeed.AddString( "12" );			break;
					case cdREL_VAL_ISO_16:		index = m_CISOSpeed.AddString( "16" );			break;
					case cdREL_VAL_ISO_20:		index = m_CISOSpeed.AddString( "20" );			break;
					case cdREL_VAL_ISO_25:		index = m_CISOSpeed.AddString( "25" );			break;
					case cdREL_VAL_ISO_32:		index = m_CISOSpeed.AddString( "32" );			break;
					case cdREL_VAL_ISO_40:		index = m_CISOSpeed.AddString( "40" );			break;
					case cdREL_VAL_ISO_50:		index = m_CISOSpeed.AddString( "50" );			break;
					case cdREL_VAL_ISO_64:		index = m_CISOSpeed.AddString( "64" );			break;
					case cdREL_VAL_ISO_80:		index = m_CISOSpeed.AddString( "80" );			break;
					case cdREL_VAL_ISO_100:		index = m_CISOSpeed.AddString( "100" );			break;
					case cdREL_VAL_ISO_125:		index = m_CISOSpeed.AddString( "125" );			break;
					case cdREL_VAL_ISO_160:		index = m_CISOSpeed.AddString( "160" );			break;
					case cdREL_VAL_ISO_200:		index = m_CISOSpeed.AddString( "200" );			break;
					case cdREL_VAL_ISO_250:		index = m_CISOSpeed.AddString( "250" );			break;
					case cdREL_VAL_ISO_320:		index = m_CISOSpeed.AddString( "320" );			break;
					case cdREL_VAL_ISO_400:		index = m_CISOSpeed.AddString( "400" );			break;
					case cdREL_VAL_ISO_500:		index = m_CISOSpeed.AddString( "500" );			break;
					case cdREL_VAL_ISO_640:		index = m_CISOSpeed.AddString( "640" );			break;
					case cdREL_VAL_ISO_800:		index = m_CISOSpeed.AddString( "800" );			break;
					case cdREL_VAL_ISO_1000:	index = m_CISOSpeed.AddString( "1000" );		break;
					case cdREL_VAL_ISO_1250:	index = m_CISOSpeed.AddString( "1250" );		break;
					case cdREL_VAL_ISO_1600:	index = m_CISOSpeed.AddString( "1600" );		break;
					case cdREL_VAL_ISO_2000:	index = m_CISOSpeed.AddString( "2000" );		break;
					case cdREL_VAL_ISO_2500:	index = m_CISOSpeed.AddString( "2500" );		break;
					case cdREL_VAL_ISO_3200:	index = m_CISOSpeed.AddString( "3200" );		break;
					case cdREL_VAL_ISO_4000:	index = m_CISOSpeed.AddString( "4000" );		break;
					case cdREL_VAL_ISO_5000:	index = m_CISOSpeed.AddString( "5000" );		break;
					case cdREL_VAL_ISO_6400:	index = m_CISOSpeed.AddString( "6400" );		break;
					case 0xffff:				index = m_CISOSpeed.AddString( "(Invalid)" );	break;
					default:					index = m_CISOSpeed.AddString( "(Error)" );	
				};
				
				m_CISOSpeed.SetItemData( index, PossibleValue );

				if(PossibleValue == CurrValue)
				{
					// Set current setting to edit control of Combo box 
					m_CISOSpeed.SetCurSel( index );
				}
			}

			m_CISOSpeed.EnableWindow( TRUE );
		}

		err = CDEnumRelCamSettingDataRelease(hEnumPossibleValue);
		hEnumPossibleValue = NULL;
		if(err) goto camerr;
		
		break;
	}

	err = CDEnumRelCamSettingRelease(hEnumCamSetting);
	hEnumCamSetting = NULL;
	if(err) goto camerr;
	
	return;

camerr:
	if(hEnumPossibleValue)
	{
		CDEnumRelCamSettingDataRelease(hEnumPossibleValue);
	}
	if(hEnumCamSetting)
	{
		CDEnumRelCamSettingRelease(hEnumCamSetting);
	}

	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );

	return;
}

void CRelCtrlDlg::SetPhotoEffectBox_CDSDK()
{
	//
	// Update Photo Effect Combo Box
	//

	cdError					err = cdOK;

	cdRelCamSettingID		TargetSettingID = cdREL_SET_PHOTO_EFFECT;
	
	cdPhotoEffect			CurrValue = cdPHOTO_EFFECT_UNKNOWN;
	cdPhotoEffect			PossibleValue = cdPHOTO_EFFECT_UNKNOWN;
	cdHEnum					hEnumCamSetting = NULL;
	cdHEnum					hEnumPossibleValue = NULL;
	cdUInt32				BufSize = 0;
	cdRelCamSettingStruct	relStruct;

	memset(&relStruct, 0, sizeof(relStruct));

	if( !m_CPhotoEffect.IsWindowVisible() )
	{
		return;
	}
	m_CPhotoEffect.EnableWindow( FALSE );
	// Remove all items from the list box.
	m_CPhotoEffect.ResetContent();

	//
	// Check whether the camera supports the Photo Effect setting. 
	err = CDEnumRelCamSettingReset( m_hSource, &hEnumCamSetting);
	if(err) goto camerr;
	
	while( (err = CDEnumRelCamSettingNext(hEnumCamSetting, &relStruct) )== cdOK)
	{
		if(relStruct.SettingID != TargetSettingID)
		{
			continue;
		}

		// Camera supports Photo Effect setting. //

		BufSize = sizeof(CurrValue);

		// Get current setting for default value.
		err = CDGetRelCamSettingData(m_hSource, TargetSettingID, &BufSize, &CurrValue);
		if(err) goto camerr;

		//
		// Get all Photo Effect settings which can be set to camera,
		// and set the list box.
		err = CDEnumRelCamSettingDataReset(m_hSource, TargetSettingID, &hEnumPossibleValue, &BufSize);
		if(err)goto camerr;

		if(BufSize == sizeof(PossibleValue))
		{	
			while( (err = CDEnumRelCamSettingDataNext(hEnumPossibleValue, sizeof(PossibleValue), &PossibleValue) ) == cdOK)
			{
				int index;
				switch(PossibleValue){
					case cdPHOTO_EFFECT_OFF:			index = m_CPhotoEffect.AddString( "Off" );				break;
					case cdPHOTO_EFFECT_VIVID:			index = m_CPhotoEffect.AddString( "Vivid" );			break;
					case cdPHOTO_EFFECT_NEUTRAL:		index = m_CPhotoEffect.AddString( "Neutral" );			break;
					case cdPHOTO_EFFECT_LOW_SHARPENING:	index = m_CPhotoEffect.AddString( "Low Sharpening" );	break;
					case cdPHOTO_EFFECT_SEPIA:			index = m_CPhotoEffect.AddString( "Sepia" );			break;
					case cdPHOTO_EFFECT_BW:				index = m_CPhotoEffect.AddString( "BW" );				break;
					case cdPHOTO_EFFECT_CUSTOM:			index = m_CPhotoEffect.AddString( "Custom" );			break;
					case cdPHOTO_EFFECT_UNKNOWN:		index = m_CPhotoEffect.AddString( "(Unknown)" );		break;
					default:							index = m_CPhotoEffect.AddString( "(Error)" );	
				};
				
				m_CPhotoEffect.SetItemData( index, PossibleValue );

				if(PossibleValue == CurrValue)
				{
					// Set current setting to edit control of Combo box 
					m_CPhotoEffect.SetCurSel( index );
				}
			}

			m_CPhotoEffect.EnableWindow( TRUE );
		}

		err = CDEnumRelCamSettingDataRelease(hEnumPossibleValue);
		hEnumPossibleValue = NULL;
		if(err) goto camerr;

		break;
	}

	err = CDEnumRelCamSettingRelease(hEnumCamSetting);
	hEnumCamSetting = NULL;
	if(err) goto camerr;
	
	return;

camerr:
	if(hEnumPossibleValue)
	{
		CDEnumRelCamSettingDataRelease(hEnumPossibleValue);
	}
	if(hEnumCamSetting)
	{
		CDEnumRelCamSettingRelease(hEnumCamSetting);
	}

	char	szErrStr[256];
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );

	return;
}

void CRelCtrlDlg::OnSelchangeShootingMode_CDSDK() 
{
	cdShootingMode ShootingMode = (cdShootingMode)m_CShootingMode.GetItemData( m_CShootingMode.GetCurSel() );

	cdError err = CDSetShootingMode(m_hSource, ShootingMode);
	if( err ) goto camerr;

	// Update other Combo Box
	SetExpoCompBox_CDSDK();
	SetFlashBox_CDSDK();
	SetISOSpeedBox_CDSDK();
	SetPhotoEffectBox_CDSDK();

	GetCameraInformation_CDSDK();

	return;

camerr:
	char szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	return;
}

void CRelCtrlDlg::OnSelchangeExpoComp_CDSDK() 
{
	cdCompensation Comp = (cdCompensation)m_CExpoComp.GetItemData( m_CExpoComp.GetCurSel() );
	
	cdError err = CDSetExposureComp(m_hSource, Comp);
	if( err ) goto camerr;
		
	// Update other Combo Box
	SetShootingModeBox_CDSDK();
	SetISOSpeedBox_CDSDK();
	SetPhotoEffectBox_CDSDK();
	SetFlashBox_CDSDK();

	GetCameraInformation_CDSDK();

	return;

camerr:
	char szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangeFlash_CDSDK() 
{
	cdFlashMode  FlashMode = (cdFlashMode)m_CFlash.GetItemData( m_CFlash.GetCurSel() );
	
	cdError err = CDSetFlashSetting(m_hSource, FlashMode, cdCOMP_NA);
	if( err ) goto camerr;

	// Update other Combo Box
	SetShootingModeBox_CDSDK();
	SetExpoCompBox_CDSDK();
	SetISOSpeedBox_CDSDK();
	SetPhotoEffectBox_CDSDK();

	GetCameraInformation_CDSDK();

	return;

camerr:
	char szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangeISOSpeed_CDSDK() 
{
	cdUInt16 ISOValue = (cdUInt16)m_CISOSpeed.GetItemData( m_CISOSpeed.GetCurSel() );
	
	cdError err = CDSetRelCamSettingData(m_hSource, cdREL_SET_ISO_SPEED_RATINGS, sizeof(ISOValue), &ISOValue);
	if( err ) goto camerr;

	// Update other Combo Box
	SetShootingModeBox_CDSDK();
	SetExpoCompBox_CDSDK();
	SetFlashBox_CDSDK();
	SetPhotoEffectBox_CDSDK();

	GetCameraInformation_CDSDK();

	return;

camerr:
	char szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
}

void CRelCtrlDlg::OnSelchangePhotoEffect_CDSDK() 
{
	cdPhotoEffect PhotoEffect = (cdPhotoEffect)m_CPhotoEffect.GetItemData( m_CPhotoEffect.GetCurSel() );
	
	cdError err = CDSetRelCamSettingData(m_hSource, cdREL_SET_PHOTO_EFFECT, sizeof(PhotoEffect), &PhotoEffect);
	if( err ) goto camerr;

	// Update other Combo Box
	SetShootingModeBox_CDSDK();
	SetExpoCompBox_CDSDK();
	SetFlashBox_CDSDK();
	SetISOSpeedBox_CDSDK();

	GetCameraInformation_CDSDK();

	return;

camerr:
	char szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
}


LRESULT CRelCtrlDlg::WindowProc_CDSDK(UINT message, WPARAM wParam, LPARAM lParam) 
{
	// TODO: Add your specialized code here and/or call the base class
	cdError		err;
	BOOL		fRes;
	CProgress	CProg;
	char		szSavePath[MAX_PATH];
	cdUInt32	NumData;
	
	if( message == m_ReleaseOnMessage )
	{
		/* UI is locked so that information may not be changed. */
		err = CDLockUI( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* It sets up so that a complete message may be disregarded. */
		m_fProgramRelease = TRUE;
	
		/* A special camera ends a view finder. */
		if( m_RelControlCap&cdRELEASE_CONTROL_CAP_ABORT_VIEWFINDER )
		{
			if( m_fVFEnd )
			{
				/* A view finder is ended. */
				err = CDTermViewfinder( m_hSource );
				if( GETERRORID(err) != cdOK )
				{
					goto	camerr;
				}
		
				/* A thread is ended. */
				m_fVFEnd = FALSE;
				m_CpVFThread_CDSDK->ResumeThread();
				WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
				
				m_CViewFinder.SetWindowText( "Start" );				
				Invalidate();
				UpdateWindow();
			}
		}
		
		/* A photograph is taken. */
		NumData = 0;		
		err = CDRelease( m_hSource, FALSE, NULL, NULL, cdPROG_NO_REPORT, &NumData );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* The directory to save is acquired. */
		GetSavePath( szSavePath, MAX_PATH );
		
		/* The photoed picture is saved. */
		fRes = CProg.GetReleaseData_CDSDK( m_hSource, NumData, szSavePath );
		if( !fRes )
		{
			goto	apierr;
		}
		else if( GETERRORID(CProg.m_LastErr_CDSDK) == cdOPERATION_CANCELLED )
		{
			m_fProgramRelease = FALSE;
			CDUnlockUI( m_hSource );
			return	TRUE;
		}
		else if( GETERRORID(CProg.m_LastErr_CDSDK) != cdOK )
		{
			err = CProg.m_LastErr_CDSDK;
			goto	camerr;
		}
		
		/* The lock of UI is canceled. */
		err = CDUnlockUI( m_hSource );
		if( GETERRORID(err) != cdOK )
		{
			goto	camerr;
		}
		
		/* It sets up so that a complete message may be received. */
		m_fProgramRelease = FALSE;
		
		return	TRUE;
	}
	else if( message == m_ReleaseCompleteMessage )
	{
		if( m_fProgramRelease == FALSE )
		{
			/* UI is locked so that information may not be changed. */
			err = CDLockUI( m_hSource );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			
			/* The directory to save is acquired. */
			GetSavePath( szSavePath, MAX_PATH );
			
			NumData = (cdUInt32)wParam;
			/* The photoed picture is saved. */
			fRes = CProg.GetReleaseData_CDSDK( m_hSource, NumData, szSavePath );
			if( !fRes )
			{
				goto	apierr;
			}
			else if( GETERRORID(CProg.m_LastErr_CDSDK) == cdOPERATION_CANCELLED )
			{
				m_fProgramRelease = FALSE;
				CDUnlockUI( m_hSource );
				return	TRUE;
			}
			else if( GETERRORID(CProg.m_LastErr_CDSDK) != cdOK )
			{
				err = CProg.m_LastErr_CDSDK;
				goto	camerr;
			}
			
			/* The lock of UI is canceled. */
			err = CDUnlockUI( m_hSource );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			
			return	TRUE;

		}
	}
	else if( message == m_AbortPCEVF )
	{
		if( m_fVFEnd )
		{
			/* A view finder is ended. */
			err = CDTermViewfinder( m_hSource );
			if( GETERRORID(err) != cdOK )
			{
				goto	camerr;
			}
			
			/* A thread is ended. */
			m_fVFEnd = FALSE;
			m_CpVFThread_CDSDK->ResumeThread();
			WaitForSingleObject( m_CpVFThread_CDSDK->m_hThread, INFINITE );
			
			m_CViewFinder.SetWindowText( "Start" );
			Invalidate();
			UpdateWindow();
		}

		return TRUE;
	}
	else if( message == m_ChangeByUI )
	{
		// Update all Combo Box.
		SetShootingModeBox_CDSDK();
		SetExpoCompBox_CDSDK();
		SetFlashBox_CDSDK();
		SetISOSpeedBox_CDSDK();
		SetPhotoEffectBox_CDSDK();
		
		GetCameraInformation_CDSDK();

		return TRUE;
	}

	return CDialog::WindowProc(message, wParam, lParam);
	
camerr:
	char	szErrStr[256];
	
	wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
	MessageBox( szErrStr );
	CDUnlockUI( m_hSource );
	m_fProgramRelease = FALSE;
	return	FALSE;
	
apierr:
	MessageBox( "API Error" );
	CDUnlockUI( m_hSource );
	m_fProgramRelease = FALSE;
	return	FALSE;
	
}

void CRelCtrlDlg::ShowReleaseParam(char* modelName) 
{
	int cmdshow;

	if( modelName == NULL							||
		!stricmp(modelName, "")						||

		!stricmp(modelName, "PowerShot G1")			||
		!stricmp(modelName, "PowerShot G2")			||
		!stricmp(modelName, "PowerShot Pro90 IS")	||
		!stricmp(modelName, "PowerShot S30")		||
		!stricmp(modelName, "PowerShot S40")		||

		!stricmp(modelName, "IXY DIGITAL")			||
		!stricmp(modelName, "PowerShot S100")		||
		!stricmp(modelName, "DIGITAL IXUS")			||

		!stricmp(modelName, "IXY DIGITAL 300")		||
		!stricmp(modelName, "PowerShot S300")		||
		!stricmp(modelName, "DIGITAL IXUS 300")		||
		
		!stricmp(modelName, "IXY DIGITAL 200")		||
		!stricmp(modelName, "PowerShot S110")		||
		!stricmp(modelName, "DIGITAL IXUS v")		||

		!stricmp(modelName, "IXY DIGITAL 300a")		||
		!stricmp(modelName, "PowerShot S300a")		||
		!stricmp(modelName, "DIGITAL IXUS 330")		||
		
		!stricmp(modelName, "IXY DIGITAL 200a")		||
		!stricmp(modelName, "PowerShot S200")		||
		!stricmp(modelName, "DIGITAL IXUS v2")		||

		!stricmp(modelName, "PowerShot A10")		||
		!stricmp(modelName, "PowerShot A20")		||
		!stricmp(modelName, "PowerShot A30")		||
		!stricmp(modelName, "PowerShot A40")		||
		!stricmp(modelName, "PowerShot A100")		||
		!stricmp(modelName, "PowerShot A200")		||

		!stricmp(modelName, "EOS D30")				||
		!stricmp(modelName, "EOS D60")				||
		!stricmp(modelName, "EOS-1Ds")				||
		!stricmp(modelName, "EOS-1D")
	  )
	{
		cmdshow = SW_HIDE;
	}
	else
	{
		cmdshow = SW_SHOW;
	}

	m_CShootingMode.ShowWindow(cmdshow);
	m_CExpoComp.ShowWindow(cmdshow);
	m_CFlash.ShowWindow(cmdshow);
	m_CISOSpeed.ShowWindow(cmdshow);
	m_CPhotoEffect.ShowWindow(cmdshow);

	m_Static_ShootingMode.ShowWindow(cmdshow);
	m_Static_ExpoComp.ShowWindow(cmdshow);
	m_Static_Flash.ShowWindow(cmdshow);
	m_Static_ISOSpeed.ShowWindow(cmdshow);
	m_Static_PhotoEffect.ShowWindow(cmdshow);
	m_Static_ReleaseParam.ShowWindow(cmdshow);

}
