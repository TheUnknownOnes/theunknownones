// RelCtrlDlg.h : header file
//

#if !defined(AFX_RELCTRLDLG_H__02FEC9E6_1C97_11D6_9CA2_002018A1F394__INCLUDED_)
#define AFX_RELCTRLDLG_H__02FEC9E6_1C97_11D6_9CA2_002018A1F394__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <afxmt.h>

#include "prType.h"
#include "prError.h"
#include "jpeglib.h"


#define		VIEWFINDER_WIDTH		320
#define		VIEWFINDER_HEIGHT		240
#define		VIEWFINDER_MUTEXNAME	"ViewFineder Mutex"

#define		FILEFORMAT_JPG			0
#define		FILEFORMAT_BMP			1

#define		CAMERAEVENTMESSAGESTRING_RELEASE_ON		"Camera Event ReleaseOn"
#define		CAMERAEVENTMESSAGESTRING_RELEASE_COMP	"Camera Event ReleaseComplete"
#define		CAMERAEVENTMESSAGESTRING_ABORT_PC_EVF	"Camera Event Abort PC EVF"
#define		CAMERAEVENTMESSAGESTRING_CHANGE_BY_UI	"Camera Event Change By UI"
#define		CAMERAEVENTMESSAGESTRING_RELEASE_DATA	"Camera Event Get Release Data"
#define		CAMERAEVENTMESSAGESTRING_SHUT_DOWN		"Camera Event Shut Down"

#ifndef		GETERRORID
#define		GETERRORID( x )		(cdERROR_ERRORID_MASK&x)
#endif

#define		MY_BUFFER_SIZE		20480L	/* 20KB */
#define		VF_BUFFER_SIZE		100 * 1024L	/* 100KB */

#define		FULL_VIEW_REL	0x0001		// Full View Release
#define		THMB_VIEW_REL	0x0002		// Thumbnail View Release

/* Information on a picture */
typedef	struct	tagDIBData
{
	HBITMAP		hBmp;
	LPVOID		vpBits;
}DIBData;

typedef struct {
	prUInt32		ContainerLength;
	prUInt16		ContainerType;
	prUInt16		Code;
	prUInt32		TransactionID;
	prUInt32		Parameter[prANY];
} EVENT_GENERIC_CONTAINER;

typedef struct{
	struct jpeg_decompress_struct	cinfo;
	struct jpeg_error_mgr			jerr;
	POINT		wh;
}PROJ;

/////////////////////////////////////////////////////////////////////////////
// CRelCtrlDlg dialog

class CRelCtrlDlg : public CDialog
{
// Construction
public:
	CRelCtrlDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CRelCtrlDlg)
	enum { IDD = IDD_RELCTRL_DIALOG };
	CStatic	m_Static_ShootingMode;
	CButton	m_Static_ReleaseParam;
	CStatic	m_Static_PhotoEffect;
	CStatic	m_Static_ISOSpeed;
	CStatic	m_Static_Flash;
	CStatic	m_Static_ExpoComp;
	CComboBox	m_CPhotoEffect;
	CComboBox	m_CShootingMode;
	CComboBox	m_CISOSpeed;
	CComboBox	m_CFlash;
	CComboBox	m_CExpoComp;
	CEdit	m_CSavePath;
	CButton	m_CBrowse;
	CButton	m_CAEAF;
	CStatic	m_CViewImage;
	CButton	m_CViewFinder;
	CButton	m_CRelease;
	CButton	m_CRelChk4;
	CButton	m_CRelChk3;
	CButton	m_CRelChk2;
	CButton	m_CEnd;
	CButton	m_CDisconnect;
	CButton	m_CConnect;
	CString	m_CInfoString;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRelCtrlDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CRelCtrlDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnConnect();
	afx_msg void OnDisconnect();
	afx_msg void OnEnd();
	afx_msg void OnRelease();
	afx_msg void OnRelcheck2();
	afx_msg void OnRelcheck3();
	afx_msg void OnRelcheck4();
	afx_msg void OnViewfinder();
	afx_msg void OnAeaf();
	afx_msg void OnBrowse();
	afx_msg void OnDestroy();
	afx_msg void OnSelchangeShootingMode();
	afx_msg void OnSelchangeExpoComp();
	afx_msg void OnSelchangeFlash();
	afx_msg void OnSelchangeISOSpeed();
	afx_msg void OnSelchangePhotoEffect();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
		
private:

	BOOL				CreateDIBBuffer();
	prResponse			SetReleaseState();
	prVoid				UpdateSetting();
	prVoid				IsSupportedCapRelPrm(prUInt8*	pDeviceInfo,
											 prBoolean*	pExpoureMode,
											 prBoolean*	pExpoureComp,
											 prBoolean*	pSTrobeSet,
											 prBoolean*	pISO,
											 prBoolean*	pPhotoEffect);
	prVoid				IsSupportedRCandEVF(prUInt8* pDeviceInfo,
											prBoolean* pbRC,
											prBoolean* pbEVF,
											prBoolean* pbAwb);
	prBoolean			IsSuppotedCapTransMode(prUInt8* pDeviceInfo,
											   prUInt16* pwCurrentValue,
											   prUInt16* pwMode);
	prBoolean			IsReleasePictureType(prUInt8* pDeviceInfo);


	BOOL				m_fVFEnd;			/* The flag of a view finder of operation */
	CStatic				*m_CpViewFinder;	/* The pointer to the class which shows the view finder picture */
	DIBData				m_BackSurface;		/* Information on a picture buffer */
	BOOL				m_fProgramRelease;	/* The release flag by the program */
	BOOL				m_fCamTypeEOS;
	BOOL				m_fMCard;

	UINT				m_ReleaseOnMessage;		/* A message when the shutter release of a camera is pushed */
	UINT				m_ReleaseCompleteMessage;/* A message when photography is completed */
	UINT				m_AbortPCEVF;
	CWinThread*			m_CpVFThread;			/* The class of a picture display thread */
	CMutex				m_CVFMutex;				/* The synchronous class of image data */
	UINT				m_ChangeByUI;			/* A message when the capture parameter of the camera is changed 
												   by operation of the camera UI. */
	UINT				m_CameraShutDown;		/*  */

	prHandle			m_hCamera;				/*	CameraHandle	*/

	prUInt8				m_ExpoCompCntVal;
	prUInt8				m_ShootingModeCntVal;
	prUInt16			m_FlashCntVal;
	prUInt16			m_PhotEffectCntVal;
	prUInt16			m_ISOCntVal;

	prObjectHandle		m_PicHandle;
	prObjectHandle		m_ThmbHandle;
	prUInt16			m_ReleaseComp;
	PROJ				m_Proj;
	prUInt8				m_ViewFinderData[VF_BUFFER_SIZE];
	prUInt32			m_VF_DataSize;
	prptpObjectFormatCode	m_FileType;
	prBoolean			m_fGetRelease;
	prBoolean			m_fInReleaseControl;

	void				ShowReleaseParam(char* modelName);
	void				SetShootingModeBox();	
	void				SetExpoCompBox();
	void				SetFlashBox();	
	void				SetISOSpeedBox();	
	void				SetPhotoEffectBox();
	prVoid				CameraDisconnect();

	void				GetSavePath(	char	*cpBuffer,
										int		iBufferSize );

	/*	PRSEK CallBack Functions & Thread Function */
	static prResponse prSTDCALL CamCallBackFunc(	prHandle		CameraHandle,
										prContext		Context,
										prVoid*			pEventData);
	static prResponse prSTDCALL ViewFinderCallBackFun(	prHandle		CameraHandle,
												prContext		Context,
												prUInt32		Size,
												prVoid*			pEventData);
	static UINT WINAPI ViewFinderProc( LPVOID	vpParam );



	/*	CDSDK DlgEvent Functions	*/
	void				OnConnect_CDSDK(cdSourceInfo* pSSrcInfo);
	void				OnDisconnect_CDSDK();
	void				OnEnd_CDSDK();
	void				OnRelease_CDSDK();
	void				OnRelcheck1_CDSDK();
	void				OnRelcheck2_CDSDK();
	void				OnRelcheck3_CDSDK();
	void				OnRelcheck4_CDSDK();
	void				OnViewfinder_CDSDK();
	void				OnAeaf_CDSDK();
	void				OnSelchangeShootingMode_CDSDK();
	void				OnSelchangeExpoComp_CDSDK();
	void				OnSelchangeFlash_CDSDK();
	void				OnSelchangeISOSpeed_CDSDK();
	void				OnSelchangePhotoEffect_CDSDK();

	/*	CDSDK Functions 	*/
	void				SetShootingModeBox_CDSDK();	
	void				SetExpoCompBox_CDSDK();
	void				SetFlashBox_CDSDK();	
	void				SetISOSpeedBox_CDSDK();	
	void				SetPhotoEffectBox_CDSDK();
	void				GetCameraInformation_CDSDK();
	void				GetCameraInformation();
	cdError				SetReleaseState_CDSDK();

	LRESULT				WindowProc_CDSDK(UINT message, WPARAM wParam, LPARAM lParam);

	/* CDSDK call back function & Thread Function */
	static cdUInt32	cdSTDCALL CamCallBackFunc_CDSDK(cdEventID		EventID,
													const cdVoid*	pData,
													cdUInt32		DataSize,
													cdContext		Context );
	static cdUInt32	cdSTDCALL RelCallBackFunc_CDSDK(cdReleaseEventID	EventID,
													const void *		pData,
													cdUInt32 			DataSize,
													cdContext			Context );
	static cdUInt32	cdSTDCALL ViewFinderCallBackFun_CDSDK(	cdVoid		*pBuf,
										 					cdUInt32	Size,	
															cdUInt32	Format,
															cdContext	Context );
	static UINT WINAPI	ViewFinderProc_CDSDK( LPVOID	vpParam );

	/*	CDSDK Menber Area	*/
	cdHSource			m_hSource;			/* The handle of a camera */
	cdReleaseControlCap m_RelControlCap;
	cdHandle			m_hCallbackFunction;	/**/
	CWinThread*			m_CpVFThread_CDSDK;		/* The class of a picture display thread */


};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RELCTRLDLG_H__02FEC9E6_1C97_11D6_9CA2_002018A1F394__INCLUDED_)
