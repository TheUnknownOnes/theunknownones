#if !defined(AFX_COMSETTINGDLG_H__A60DA218_0CFB_46E1_BC84_9AB9693F5E44__INCLUDED_)
#define AFX_COMSETTINGDLG_H__A60DA218_0CFB_46E1_BC84_9AB9693F5E44__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ComSettingDlg.h : header file
//

#include "prAPI.h"
#include "cdAPI.h"

#define		CAMERA_ENUM_PRSDK	0x00000001		/* Camara devices enumerated by PS-ReC SDK */
#define		CAMERA_ENUM_CDSDK	0x00000002		/* Camera devices enumerated by CD-SDK */

/* Camera device information (PS-ReC SDK and CD-SDK) */
typedef struct sdk_and_info
{
	prUInt32				m_SelectedSDK;		/* CAMERA_ENUM_PRSDK or CAMERA_ENUM_CDSDK */
	union
	{
		prDeviceInfoTable	SelectedSrc_PRSDK;	/* Camera device information (PS-ReC SDK) */
		cdSourceInfo		SelectedSrc_CDSDK;	/* Camera device information (CD-SDK) */
	} SrcInfo;
} SDK_AND_INFO;

/////////////////////////////////////////////////////////////////////////////
// CComSettingDlg dialog

class CComSettingDlg : public CDialog
{
// Construction
public:
	CComSettingDlg(CWnd* pParent = NULL);   // standard constructor
	void SelectSource( SDK_AND_INFO* pSdk_and_Info );

// Dialog Data
	//{{AFX_DATA(CComSettingDlg)
	enum { IDD = IDD_COM_SETTING };
	CComboBox	m_CameraList;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CComSettingDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CComSettingDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
	cdError		Enum_CDSDK(	cdUInt32*	pCount,
							cdUInt32 	NumList_PRSDK );
private:
	SDK_AND_INFO	m_Sdk_and_Info;
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_COMSETTINGDLG_H__A60DA218_0CFB_46E1_BC84_9AB9693F5E44__INCLUDED_)
