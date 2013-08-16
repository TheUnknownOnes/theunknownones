{-----------------------------------------------------------------------------
 Project: GroupListView_D100R
 Purpose: Defines basic types for working with the GroupListView
 Created: 22.05.2008 14:36:32
 
 (c) by TheUnknownOnes under Apache License 2.0
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}


unit uGroupListViewHeader;

interface

uses
  Classes, CommCtrl, XPMan, Windows;

type
  TLVGroup = record
    cbSize                : Cardinal;  
    mask                  : Cardinal;
    pszHeader             : PWideChar;
    cchHeader             : Integer;
    pszFooter             : PWideChar;
    cchFooter             : Integer;
    iGroupId              : Integer;
    stateMask             : Cardinal;
    state                 : Cardinal;
    uAlign                : Cardinal;
    pszSubtitle           : PWideChar;
    cchSubtitle           : Cardinal;
    pszTask               : PWideChar;
    cchTask               : Cardinal;
    pszDescriptionTop     : PWideChar;
    cchDescriptionTop     : Cardinal;
    pszDescriptionBottom  : PWideChar;
    cchDescriptionBottom  : Cardinal;
    iTitleImage           : Integer;
    iExtendedImage        : Integer;
    iFirstItem            : Integer;         // Read only
    cItems                : Cardinal;             // Read only
    pszSubsetTitle        : PWideChar;     // NULL if group is not subset
    cchSubsetTitle        : Cardinal;
  end;
  PLVGroup = ^TLVGroup;

  TLVGroupMetrics= record
    cbSize    : Cardinal;
    mask      : Cardinal;
    Left      : Cardinal;
    Top       : Cardinal;
    Right     : Cardinal;
    Bottom    : Cardinal;
    crLeft    : COLORREF;
    crTop     : COLORREF;
    crRight   : COLORREF;
    crBottom  : COLORREF;
    crHeader  : COLORREF;
    crFooter  : COLORREF;
  end;
  PLVGroupMetrics = ^TLVGroupMetrics;


  TLVItem = record
    mask        : Cardinal;
    iItem       : Integer;
    iSubItem    : Integer;
    state       : Cardinal;
    stateMask   : Cardinal;
    pszText     : PChar;
    cchTextMax  : Integer;
    iImage      : Integer;
    lParam      : LPARAM;
    iIndent     : Integer;
    //the following is the new part
    iGroupId    : Integer;
    cColumns    : Cardinal;
    puColumns   : PCardinal;
  end;
  PLVItem = ^TLVItem;

  TFNLVGroupCompare = procedure(Group1_ID,
                                Group2_ID : Integer;
                                pvData : Pointer);
  PFNLVGroupCompare = ^TFNLVGroupCompare;

  TLVInsertGroupSorted = record
    pfnGroupCompare : PFNLVGroupCompare;
    pvData          : Pointer;
    lvGroup         : TLVGroup;
  end;
  PLVInsertGroupSorted = ^TLVInsertGroupSorted;

  TLVTileViewInfo = record
    cbSize        : Cardinal;
    dwMask        : LongWord;     //LVTVIM_*
    dwFlags       : LongWord;    //LVTVIF_*
    sizeTile      : TSize;
    cLines        : Integer;
    rcLabelMargin : TRect;
  end;
  PLVTileViewInfo = ^TLVTileViewInfo;

  TLVTileInfo = record
    cbSize    : Cardinal;
    iItem     : Integer;
    cColumns  : Cardinal;
    puColumns : PCardinal;
  end;
  PLVTileInfo = ^TLVTileInfo;

const
  LVGF_NONE           = $00000000;
  LVGF_HEADER         = $00000001;
  LVGF_FOOTER         = $00000002;
  LVGF_STATE          = $00000004;
  LVGF_ALIGN          = $00000008;
  LVGF_GROUPID        = $00000010;
  LVGF_SUBTITLE       = $00000020; //todo: get right values
  LVGF_TASK           = $00000040;
  LVGF_DESCRIPTIONTOP = $00000080;
  LVGF_DESCRIPTIONBOTTOM = $00000100;
  LVGF_TITLEIMAGE     = $00000200;
  LVGF_EXTENDEDIMAGE  = $00000400;
  LVGF_ITEMS          = $00000800;
  LVGF_SUBSET         = $00001000;
  LVGF_SUBSETITEMS    = $00002000;

  LVGS_NORMAL         = $00000000;
  LVGS_COLLAPSED      = $00000001;
  LVGS_HIDDEN         = $00000002;
  LVGS_NOHEADER       = $00000004; //todo: get right values
  LVGS_COLLAPSIBLE    = $00000008;
  LVGS_FOCUSED        = $00000010;
  LVGS_SELECTED       = $00000020;
  LVGS_SUBSETED       = $00000040;
  LVGS_SUBSETLINKFOCUSED = $00000080;

  LVGA_HEADER_LEFT    = $00000001;
  LVGA_HEADER_CENTER  = $00000002;
  LVGA_HEADER_RIGHT   = $00000004;  // Don't forget to validate exclusivity
  LVGA_FOOTER_LEFT    = $00000008;
  LVGA_FOOTER_CENTER  = $00000010;
  LVGA_FOOTER_RIGHT   = $00000020;  // Don't forget to validate exclusivity

  LVGMF_NONE          = $00000000;
  LVGMF_BORDERSIZE    = $00000001;
  LVGMF_BORDERCOLOR   = $00000002;
  LVGMF_TEXTCOLOR     = $00000004;

  LVTVIF_AUTOSIZE     = $00000000;
  LVTVIF_FIXEDWIDTH   = $00000001;
  LVTVIF_FIXEDHEIGHT  = $00000002;
  LVTVIF_FIXEDSIZE    = $00000003;

  LVIF_GROUPID        = $00000100;
  LVIF_COLUMNS        = $00000200;

  LVTVIM_TILESIZE     = $00000001;
  LVTVIM_COLUMNS      = $00000002;
  LVTVIM_LABELMARGIN  = $00000004;

  LVS_EX_LABELTIP               = $00004000; // listview unfolds partly hidden labels if it does not have infotip text
  LVS_EX_BORDERSELECT           = $00008000; // border selection style instead of highlight
  LVS_EX_DOUBLEBUFFER           = $00010000;
  LVS_EX_HIDELABELS             = $00020000;
  LVS_EX_SINGLEROW              = $00040000;
  LVS_EX_SNAPTOGRID             = $00080000;  // Icons automatically snap to grid.
  LVS_EX_SIMPLESELECT           = $00100000;
  LVS_EX_JUSTIFYCOLUMNS         = $00200000;
  LVS_EX_TRANSPARENTBKGND       = $00400000;
  LVS_EX_TRANSPARENTSHADOWTEXT  = $00800000;
  LVS_EX_AUTOAUTOARRANGE        = $01000000;
  LVS_EX_HEADERINALLVIEWS       = $02000000;
  LVS_EX_AUTOCHECKSELECT        = $08000000;
  LVS_EX_AUTOSIZECOLUMNS        = $10000000;
  LVS_EX_COLUMNSNAPPOINTS       = $40000000;


  LVM_SETITEMA          = LVM_FIRST + 6;
  LVM_INSERTGROUP       = LVM_FIRST + 145;
  LVM_SETGROUPINFO      = LVM_FIRST + 147;
  LVM_GETGROUPINFO      = LVM_FIRST + 149;
  LVM_REMOVEGROUP       = LVM_FIRST + 150;
  LVM_MOVEGROUP         = LVM_FIRST + 151;
  LVM_MOVEITEMTOGROUP   = LVM_FIRST + 154;
  LVM_SETGROUPMETRICS   = LVM_FIRST + 155;
  LVM_GETGROUPMETRICS   = LVM_FIRST + 156;
  LVM_ENABLEGROUPVIEW   = LVM_FIRST + 157;
  LVM_SORTGROUPS        = LVM_FIRST + 158;
  LVM_INSERTGROUPSORTED = LVM_FIRST + 159;
  LVM_REMOVEALLGROUPS   = LVM_FIRST + 160;
  LVM_HASGROUP          = LVM_FIRST + 161;
  LVM_SETTILEVIEWINFO   = LVM_FIRST + 162;
  LVM_GETTILEVIEWINFO   = LVM_FIRST + 163;
  LVM_SETTILEINFO       = LVM_FIRST + 164;
  LVM_GETTILEINFO       = LVM_FIRST + 165;


  function ListView_InsertGroup(AListViewHandle : HWND;
                                AIndex          : Integer;
                                AGroup          : PLVGroup) : Integer;
  function ListView_SetGroupInfo(AListViewHandle  : HWND;
                                 AGroupId         : Integer;
                                 AGroup           : PLVGroup) : Integer;
  function ListView_GetGroupInfo(AListViewHandle : HWND;
                                 AGroupId        : Integer;
                                 AGroup          : PLVGroup) : Integer;
  function ListView_RemoveGroup(AListViewHandle : HWND;
                                AGroupId        : Integer) : Integer;
  function ListView_MoveGroup(AListViewHandle : HWND;
                               AGroupId        : Integer;
                               AtoIndex        : Integer) : Integer;
  procedure ListView_MoveItemToGroup(AListViewHandle  : HWND;
                                     AItemID          : Integer;
                                     AGroupID         : Integer);
  procedure ListView_SetGroupMetrics(AListViewHandle  : HWND;
                                     AGroupMetrics    : PLVGroupMetrics);
  procedure ListView_GetGroupMetrics(AListViewHandle : HWND;
                                     AGroupMetrics   : PLVGroupMetrics);
  function ListView_EnableGroupView(AListViewHandle : HWND;
                                    AEnable         : BOOL) : Integer;
  function ListView_SortGroups(AListViewHandle    : HWND;
                               AGroupCompareProc  : PFNLVGroupCompare;
                               AData              : Pointer) : Integer;
  procedure ListView_InsertGroupSorted(AListViewHandle : HWND;
                                       AInsertStruct   : PLVInsertGroupSorted);
  procedure ListView_RemoveAllGroups(AListViewHandle : HWND);
  function ListView_HasGroup(AListViewHandle : HWND;
                             AGroupId        : Integer) : BOOL;
  function ListView_SetTileViewInfo(AListViewHandle : HWND;
                                    ATileViewInfo   : PLVTileViewInfo) : BOOL;
  procedure ListView_GetTileViewInfo(AListViewHandle  : HWND;
                                     ATileViewInfo    : PLVTileViewInfo);
  function ListView_SetTileInfo(AListViewHandle : HWND;
                                ATileInfo       : PLVTileInfo) : BOOL;
  procedure ListView_GetTileInfo(AListViewHandle : HWND;
                                 ATileInfo : PLVTileInfo);

  //helper
  function GetNewLVGroup : PLVGroup;
  function GetNewLVGroupMetrics : PLVGroupMetrics;
  function GetNewLVItem : PLVItem;
  function GetNewLVInsertGroupSorted : PLVInsertGroupSorted;
  function GetNewLVTileViewInfo : PLVTileViewInfo;
  function GetNewLVTileInfo : PLVTileInfo;

implementation



function ListView_InsertGroup(AListViewHandle : HWND;
                              AIndex          : Integer;
                              AGroup          : PLVGroup) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_INSERTGROUP,
                      WParam(AIndex),
                      LParam(AGroup));
end;

function ListView_SetGroupInfo(AListViewHandle  : HWND;
                               AGroupId         : Integer;
                               AGroup           : PLVGroup) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_SETGROUPINFO,
                      WParam(AGroupId),
                      LParam(AGroup));
end;

function ListView_GetGroupInfo(AListViewHandle : HWND;
                               AGroupId        : Integer;
                               AGroup          : PLVGroup) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_GETGROUPINFO,
                      WParam(AGroupId),
                      LParam(AGroup));
end;

function ListView_RemoveGroup(AListViewHandle : HWND;
                              AGroupId        : Integer) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_REMOVEGROUP,
                      WParam(AGroupId),
                      0);
end;

function ListView_MoveGroup(AListViewHandle : HWND;
                             AGroupId        : Integer;
                             AtoIndex        : Integer) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_MOVEGROUP,
                      WParam(AGroupId),
                      LParam(AtoIndex));
end;

procedure ListView_MoveItemToGroup(AListViewHandle  : HWND;
                                   AItemID          : Integer;
                                   AGroupID         : Integer);
begin
  SendMessage(AListViewHandle,
              LVM_MOVEITEMTOGROUP,
              WParam(AItemID), 
              LParam(AGroupID));
end;

procedure ListView_SetGroupMetrics(AListViewHandle  : HWND;
                                   AGroupMetrics    : PLVGroupMetrics);
begin
  SendMessage(AListViewHandle,
              LVM_SETGROUPMETRICS,
              0,
              LParam(AGroupMetrics));
end;

procedure ListView_GetGroupMetrics(AListViewHandle : HWND;
                                   AGroupMetrics   : PLVGroupMetrics);
begin
  SendMessage(AListViewHandle,
              LVM_GETGROUPMETRICS,
              0,
              LParam(AGroupMetrics));
end;

function ListView_EnableGroupView(AListViewHandle : HWND;
                                  AEnable         : BOOL) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_ENABLEGROUPVIEW,
                      WParam(AEnable),
                      0);
end;

function ListView_SortGroups(AListViewHandle    : HWND;
                             AGroupCompareProc  : PFNLVGroupCompare;
                             AData              : Pointer) : Integer;
begin
  Result:=SendMessage(AListViewHandle,
                      LVM_SORTGROUPS,
                      WParam(AGroupCompareProc),
                      LParam(AData));
end;

procedure ListView_InsertGroupSorted(AListViewHandle : HWND;
                                     AInsertStruct   : PLVInsertGroupSorted);
begin
  SendMessage(AListViewHandle,
              LVM_INSERTGROUPSORTED,
              WParam(AInsertStruct),
              0);
end;

procedure ListView_RemoveAllGroups(AListViewHandle : HWND);
begin
  SendMessage(AListViewHandle,
              LVM_REMOVEALLGROUPS,
              0,
              0);
end;

function ListView_HasGroup(AListViewHandle : HWND;
                           AGroupId        : Integer) : BOOL;
begin
  Result:=Bool(SendMessage(AListViewHandle,
                           LVM_HASGROUP,
                           WParam(AGroupId),
                           0));
end;

function ListView_SetTileViewInfo(AListViewHandle : HWND;
                                  ATileViewInfo   : PLVTileViewInfo) : BOOL;
begin
  Result:=Bool(SendMessage(AListViewHandle,
                           LVM_SETTILEVIEWINFO,
                           0,
                           LParam(ATileViewInfo)));
end;

procedure ListView_GetTileViewInfo(AListViewHandle  : HWND;
                                   ATileViewInfo    : PLVTileViewInfo);
begin
  SendMessage(AListViewHandle,
              LVM_GETTILEVIEWINFO,
              0,
              LParam(ATileViewInfo));
end;

function ListView_SetTileInfo(AListViewHandle : HWND;
                              ATileInfo       : PLVTileInfo) : BOOL;
begin
  Result:=Bool(SendMessage(AListViewHandle,
                           LVM_SETTILEINFO,
                           0,
                           LParam(ATileInfo)));
end;

procedure ListView_GetTileInfo(AListViewHandle : HWND;
                               ATileInfo : PLVTileInfo);
begin
  SendMessage(AListViewHandle,
              LVM_GETTILEINFO,
              0,
              LParam(ATileInfo));
end;

function GetNewLVGroup : PLVGroup;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVGroup));
  Result^.cbSize:=SizeOf(TLVGroup);
end;

function GetNewLVGroupMetrics : PLVGroupMetrics;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVGroupMetrics));
  Result^.cbSize:=SizeOf(TLVGroupMetrics);
end;

function GetNewLVItem : PLVItem;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVItem));
end;

function GetNewLVInsertGroupSorted : PLVInsertGroupSorted;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVInsertGroupSorted));
end;

function GetNewLVTileViewInfo : PLVTileViewInfo;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVTileViewInfo));
  Result^.cbSize:=SizeOf(TLVTileViewInfo);
end;

function GetNewLVTileInfo : PLVTileInfo;
begin
  New(Result);
  ZeroMemory(Result,SizeOf(TLVTileInfo));
  Result^.cbSize:=SizeOf(TLVTileInfo)
end;

end.
