object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Custom Help'
  ClientHeight = 539
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 504
    Width = 498
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 339
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 420
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 175
    Width = 498
    Height = 42
    Align = alTop
    Caption = 'Help Display'
    TabOrder = 1
    object cbcusthelpwp: TCheckBox
      Left = 8
      Top = 16
      Width = 438
      Height = 18
      Caption = 
        'use WelcomePage to display help (otherwise system browser is use' +
        'd)'
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 498
    Height = 166
    Align = alTop
    Caption = 'Help Namespaces'
    TabOrder = 2
    object ListView2: TListView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 488
      Height = 143
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Namespace'
          Width = 200
        end
        item
          AutoSize = True
          Caption = 'Description'
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 223
    Width = 498
    Height = 275
    Align = alClient
    Caption = 'Other Help Sources'
    TabOrder = 3
    object ListView1: TListView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 488
      Height = 180
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 100
        end
        item
          Caption = 'Description'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'URL (Keyword will be appended)'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = ListView1Change
      OnDblClick = ListView1DblClick
      OnKeyDown = ListView1KeyDown
    end
    object Panel2: TPanel
      Left = 2
      Top = 201
      Width = 494
      Height = 72
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 6
        Top = 6
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Label2: TLabel
        Left = 6
        Top = 29
        Width = 53
        Height = 13
        Caption = 'Description'
      end
      object Label3: TLabel
        Left = 6
        Top = 52
        Width = 45
        Height = 13
        Caption = 'URL/Path'
      end
      object edName: TEdit
        Left = 65
        Top = 3
        Width = 415
        Height = 21
        TabOrder = 0
        OnChange = edNameChange
      end
      object edDesc: TEdit
        Left = 65
        Top = 26
        Width = 415
        Height = 21
        TabOrder = 1
        OnChange = edDescChange
      end
      object edURL: TEdit
        Left = 65
        Top = 48
        Width = 415
        Height = 21
        Hint = 
          '- URL to a webbased search provider (e.g. koders.com)'#13#10'- Path to' +
          ' a windows *.hlp file (be aware to have winhlp32.exe installed)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = edURLChange
      end
    end
  end
end
