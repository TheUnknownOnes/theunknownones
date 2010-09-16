object ch2FormConfigHTMLHelp: Tch2FormConfigHTMLHelp
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure HTML Help'
  ClientHeight = 393
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 559
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 19
      Align = alLeft
      Caption = 'Priority:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object ed_Prio: TSpinEdit
      Left = 54
      Top = 3
      Width = 59
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = ed_PrioChange
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 362
    Width = 559
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 481
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 553
    Height = 331
    Align = alClient
    Caption = 'Items'
    TabOrder = 2
    object LV: TListView
      AlignWithMargins = True
      Left = 5
      Top = 54
      Width = 543
      Height = 215
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Filename'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = LVSelectItem
    end
    object Panel2: TPanel
      Left = 2
      Top = 272
      Width = 549
      Height = 57
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 9
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label3: TLabel
        Left = 217
        Top = 6
        Width = 46
        Height = 13
        Caption = 'Filename:'
      end
      object Label8: TLabel
        Left = 46
        Top = 57
        Width = 3
        Height = 13
        WordWrap = True
      end
      object ed_Name: TEdit
        Left = 46
        Top = 3
        Width = 154
        Height = 21
        TabOrder = 0
        OnChange = ed_NameChange
      end
      object ed_File: TEdit
        Left = 269
        Top = 3
        Width = 250
        Height = 21
        TabOrder = 1
        OnChange = ed_FileChange
      end
      inline frame_Deco: Tch2FrameHelpItemDecoration
        Left = 0
        Top = 29
        Width = 549
        Height = 28
        Align = alBottom
        TabOrder = 2
        TabStop = True
        ExplicitTop = 29
        ExplicitWidth = 549
        inherited Label1: TLabel
          Left = 270
          ExplicitLeft = 270
        end
        inherited Label2: TLabel
          Left = 134
          ExplicitLeft = 134
        end
        inherited lbl_Caption: TLabel
          Width = 125
          Caption = 'Sample entry'
          ExplicitWidth = 63
        end
        inherited cb_Bold: TCheckBox
          Left = 406
          Width = 31
          ExplicitLeft = 406
          ExplicitWidth = 31
        end
        inherited cb_Italic: TCheckBox
          Left = 443
          ExplicitLeft = 443
        end
        inherited cb_Underline: TCheckBox
          Left = 479
          Width = 31
          ExplicitLeft = 479
          ExplicitWidth = 31
        end
        inherited cb_Strike: TCheckBox
          Left = 516
          ExplicitLeft = 516
        end
        inherited cob_Text: TColorBox
          Left = 166
          ExplicitLeft = 166
        end
        inherited cob_Back: TColorBox
          Left = 302
          ExplicitLeft = 302
        end
      end
      object btn_FindFile: TButton
        Left = 525
        Top = 3
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 3
        OnClick = btn_FindFileClick
      end
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 543
      Height = 30
      ButtonHeight = 30
      ButtonWidth = 31
      Caption = 'ToolBar1'
      Images = ch2Data.ch2Images24
      TabOrder = 2
      object btn_Add: TToolButton
        Left = 0
        Top = 0
        Caption = 'btn_Add'
        ImageIndex = 1
        OnClick = btn_AddClick
      end
      object btn_Del: TToolButton
        Left = 31
        Top = 0
        Caption = 'btn_Del'
        ImageIndex = 2
        OnClick = btn_DelClick
      end
    end
  end
  object dlg_SelectFile: TOpenDialog
    DefaultExt = '.chm'
    Filter = '.chm - Files|*.chm'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select HTML-Help-File'
    Left = 272
    Top = 200
  end
end
