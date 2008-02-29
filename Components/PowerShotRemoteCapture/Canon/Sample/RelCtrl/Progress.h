#if !defined(AFX_PROGRESS_H__37088960_1CAB_11D6_9CA2_002018A1F394__INCLUDED_)
#define AFX_PROGRESS_H__37088960_1CAB_11D6_9CA2_002018A1F394__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Progress.h : header file
//

#include "prType.h"
#include "cdType.h"

/////////////////////////////////////////////////////////////////////////////
// CProgress dialog

class CProgress : public CDialog
{
// Construction
public:
	CProgress(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CProgress)
	enum { IDD = IDD_PROGRESS };
	CProgressCtrl	m_CProgress;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProgress)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CProgress)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
	prUInt32		m_SelectedSDK;			/* PS-ReC SDK or CD-SDK */
public:
	char			m_szSavePath[MAX_PATH];
	char			m_szFileName[MAX_PATH];
public:
	void			SetProgressPos( int	iPos );

private:
	static prResponse prSTDCALL ReleaseProgressFunc(	prHandle       CameraHandle,
														prObjectHandle ObjectHandle,
														prContext      Context,
														prProgress*    pProgress	);

	static UINT AFX_CDECL	ReleaseThreadProc( LPVOID	lpParameter );

public:
	UINT					m_ThreadEndMessage;
	prResponse				m_LastErr;
	prHandle        		m_CameraHandle;
	prObjectHandle  		m_ObjectHandle;
	prptpEventCode  		m_EventCode;
	prptpObjectFormatCode	m_SavedFileFormat;
	CFile*					m_pCFile;
	CWinThread*				m_ReleaseThread;

	BOOL					GetReleaseData(	prHandle        		CameraHandle,
											prObjectHandle  		ObjectHandle,
											prptpEventCode  		EventCode,
											prptpObjectFormatCode	SavedFileFormat,
											char					*szSavePath);

/* The fhunctions uesd in CD-SDK */
private:
	static cdUInt32 cdSTDCALL ReleaseProgressFunc_CDSDK(	cdUInt32			Progress,
															cdProgressStatus	Status,
															cdContext			Context );

	static UINT 	ReleaseThreadProc_CDSDK( LPVOID	lpParameter );

public:
	UINT			m_ThreadEndMessage_CDSDK;
	cdHSource		m_hSource_CDSDK;
	cdError			m_LastErr_CDSDK;
	cdUInt32		m_NumData_CDSDK;
	CWinThread*		m_ReleaseThread_CDSDK;

	BOOL			GetReleaseData_CDSDK( cdHSource	hSource,
										  cdUInt32	NumData,
										  char		*szSavePath );

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROGRESS_H__37088960_1CAB_11D6_9CA2_002018A1F394__INCLUDED_)
