// ComSettingDlg.cpp : implementation file
//

#include "stdafx.h"
#include "RelCtrl.h"
#include "ComSettingDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CComSettingDlg dialog


CComSettingDlg::CComSettingDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CComSettingDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CComSettingDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CComSettingDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CComSettingDlg)
	DDX_Control(pDX, IDC_COMBO_CAMERA, m_CameraList);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CComSettingDlg, CDialog)
	//{{AFX_MSG_MAP(CComSettingDlg)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CComSettingDlg message handlers

void CComSettingDlg::SelectSource( SDK_AND_INFO* pSdk_and_Info )
{
	DoModal();

	memcpy( pSdk_and_Info, &m_Sdk_and_Info, sizeof(SDK_AND_INFO) );
}

void CComSettingDlg::OnOK() 
{
	// TODO: Add extra validation here
	SDK_AND_INFO* pInfo = (SDK_AND_INFO*)m_CameraList.GetItemDataPtr( m_CameraList.GetCurSel() );
	if(pInfo && (int)pInfo != CB_ERR)
	{
		memcpy( &m_Sdk_and_Info, pInfo, sizeof(SDK_AND_INFO) );
	}

	CDialog::OnOK();
}

BOOL CComSettingDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// TODO: Add extra initialization here

	memset( &m_Sdk_and_Info, 0, sizeof(SDK_AND_INFO) );
	m_CameraList.ResetContent();

	prResponse		err = prOK;
	prUInt32 		count = 0L;
	prUInt32		index;
	prUInt32		BufferSize = 0L;
	prDeviceList*	pGetDevList = NULL;

	/* Allocate memory 10 devices */
	BufferSize = (prUInt32)sizeof(prDeviceList) + (prUInt32)(sizeof(prDeviceInfoTable) * 9L);
	for(;;)
	{
		pGetDevList = (prDeviceList*)new prUInt8[BufferSize];
		/* Enumerate camera devices by PS-ReC SDK */
		err = PR_GetDeviceList( &BufferSize, (prDeviceList*)pGetDevList );
		if(!err)
		{
			break;
		}
		if((err & prERROR_ERRORID_MASK) != prINSUFFICIENT_BUFFER)
		{
			goto ErrHandler;
		}
		/* If error is prINSUFFICIENT_BUFFER, */
		/* then change the buffer size. */
		delete [] pGetDevList;
		pGetDevList = NULL;
	}

	if(pGetDevList->NumList > 0)
	{	
	    for(index = 0; index<(pGetDevList->NumList); index++)
		{
			SDK_AND_INFO* pDeviceInfo = new SDK_AND_INFO;
			char MBModelName[40];
			pDeviceInfo->m_SelectedSDK = CAMERA_ENUM_PRSDK;
			memcpy( &((pDeviceInfo->SrcInfo).SelectedSrc_PRSDK), &(pGetDevList->DeviceInfo[index]), sizeof(prDeviceInfoTable) );
			WideCharToMultiByte(CP_ACP, 0, (pDeviceInfo->SrcInfo).SelectedSrc_PRSDK.ModelName, -1, MBModelName, 40-1, 0, 0);
			m_CameraList.InsertString(index, (const char *)MBModelName);
			m_CameraList.SetItemDataPtr( index, pDeviceInfo );
		}
    }

	/* Enumerate camera devices by CD-SDK */
	err = (prResponse)Enum_CDSDK( (cdUInt32*)&count, (cdUInt32)pGetDevList->NumList );
	if(err)
	{
		goto ErrHandler;
	}

	if((pGetDevList->NumList == 0) && (count == 0))
	{
		m_CameraList.InsertString( 0, "(None)" );
	}

	m_CameraList.SetCurSel( 0 );

ErrHandler:

    if(pGetDevList)
        delete [] pGetDevList;

	if(err)
	{
		char	szErrStr[256];
		wsprintf( szErrStr, "ErrorCode = 0x%08X", err );
		MessageBox( szErrStr );

		EndDialog(0);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CComSettingDlg::OnDestroy() 
{
	CDialog::OnDestroy();

	int count = m_CameraList.GetCount();

	SDK_AND_INFO* pInfo;
	for(int index = 0; index < count; index++)
	{
		pInfo = (SDK_AND_INFO*)m_CameraList.GetItemDataPtr( index );
		if(pInfo && (int)pInfo != CB_ERR)
		{
			delete pInfo;
		}
	}
}

/* Enumerate camera devices by CD-SDK */
cdError	CComSettingDlg::Enum_CDSDK(	cdUInt32 	*pCount,
									cdUInt32 	NumList_PRSDK )
{
	// TODO: Add extra initialization here

	cdError		err = cdOK;
	cdHEnum		hEnumDevice = 0;
	cdUInt32 	count = 0L, ListCnt = 0;
	cdUInt32	index;

    err = CDEnumDeviceReset(1, &hEnumDevice);
    if(err) goto Cleanup;

    err = CDGetDeviceCount(hEnumDevice, &count);
    if(err)
	{
		goto Cleanup;
	}
	else if(count == 0L)
	{
		err = cdOK;
		goto Cleanup;
	}
	else
	{	
		ListCnt = 0L;
		SDK_AND_INFO* pSourceInfo;
	    for(index = 0; index<count; index++)
		{
			pSourceInfo = new SDK_AND_INFO;
			err = CDEnumDeviceNext( hEnumDevice, &((pSourceInfo->SrcInfo).SelectedSrc_CDSDK) );
			if(!err)
			{
				/* If OS is Windows XP,camera devices enumerated by PS-ReC SDK are enumerated by CD-SDK. */
				/* remove camera deivces enumerated by PS-ReC SDK */
				if ( m_CameraList.FindString( -1, (LPCTSTR)(pSourceInfo->SrcInfo).SelectedSrc_CDSDK.Name ) == LB_ERR )
				{
					pSourceInfo->m_SelectedSDK = CAMERA_ENUM_CDSDK;
					m_CameraList.InsertString( (NumList_PRSDK+ListCnt), (pSourceInfo->SrcInfo).SelectedSrc_CDSDK.Name );
					m_CameraList.SetItemDataPtr( (NumList_PRSDK+ListCnt), pSourceInfo );
					ListCnt++;
				}
				else
				{
					delete pSourceInfo;
				}
			}
			else
			{
				delete pSourceInfo;
			}
		}
    }

Cleanup:

    if(hEnumDevice)
        CDEnumDeviceRelease(hEnumDevice);

	if(!err && pCount)
		*pCount = ListCnt;

	return err;
}

