object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Custom Help'
  ClientHeight = 666
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 809
    Top = 3
    Width = 72
    Height = 660
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 66
      Height = 26
      Align = alTop
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 3
      Top = 35
      Width = 66
      Height = 26
      Align = alTop
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel5: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 800
    Height = 660
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object grpHelpNamespaces: TGroupBox
      Left = 0
      Top = 81
      Width = 800
      Height = 214
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'Help Namespaces'
      TabOrder = 0
      object lvNamespaces: TListView
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 790
        Height = 148
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
          end
          item
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Panel4: TPanel
        Left = 2
        Top = 169
        Width = 796
        Height = 43
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label4: TLabel
          Left = 6
          Top = 22
          Width = 176
          Height = 13
          Caption = 'Trim namespaces until result is found'
        end
        object cbFullTextSearch: TCheckBox
          Left = 6
          Top = 3
          Width = 579
          Height = 17
          Caption = 
            'Perform fulltext search ... this may be slow (if unchecked only ' +
            'index searches are performed)'
          TabOrder = 0
        end
        object cbTrimNamespacesHX: TComboBox
          Left = 192
          Top = 19
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
      end
    end
    object grpHelpDisplay: TGroupBox
      Left = 0
      Top = 0
      Width = 800
      Height = 81
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alTop
      Caption = 'Help Display'
      TabOrder = 1
      DesignSize = (
        800
        81)
      object Label5: TLabel
        Left = 8
        Top = 57
        Width = 84
        Height = 13
        Caption = 'Redirect schemes'
      end
      object cbcusthelpwp: TCheckBox
        Left = 8
        Top = 16
        Width = 438
        Height = 18
        Caption = 
          'use WelcomePage to display help (otherwise system browser is use' +
          'd)'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object cbReplaceDefaultViewer: TCheckBox
        Left = 8
        Top = 34
        Width = 438
        Height = 18
        Caption = 'replace default viewer for MS Help2 topics'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object edRedirectSchemes: TEdit
        Left = 98
        Top = 53
        Width = 697
        Height = 21
        Hint = 
          'Specify the schemes to show with the WelcomePage/system browser.' +
          #13#10'Use '#39';'#39' as delimiter, e.g. file://;http://'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
    end
    object grpOtherHelpSources: TGroupBox
      Left = 0
      Top = 295
      Width = 800
      Height = 365
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      Caption = 'Other Help Sources'
      TabOrder = 2
      object ListView1: TListView
        AlignWithMargins = True
        Left = 5
        Top = 64
        Width = 790
        Height = 202
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
            Caption = 'URL ... $(HelpString) will be replaced by Keyword'
          end
          item
            Caption = 'Trim Namespaces'
            Width = 80
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ListView1Change
        OnDblClick = ListView1DblClick
        OnInfoTip = ListView1InfoTip
        OnKeyDown = ListView1KeyDown
      end
      object pnlOHSItem: TPanel
        Left = 2
        Top = 269
        Width = 796
        Height = 94
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          796
          94)
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
        object Label8: TLabel
          Left = 6
          Top = 73
          Width = 82
          Height = 13
          Caption = 'Trim namespaces'
        end
        object edName: TEdit
          Left = 94
          Top = 3
          Width = 700
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 0
          OnChange = edNameChange
        end
        object edDesc: TEdit
          Left = 94
          Top = 26
          Width = 700
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 1
          OnChange = edDescChange
        end
        object edURL: TEdit
          Left = 94
          Top = 48
          Width = 700
          Height = 21
          Hint = 
            '- URL to a webbased search provider (e.g. koders.com)'#13#10'- Path to' +
            ' a windows *.hlp file (be aware to have winhlp32.exe installed)'#13 +
            #10'- Path to a windows htmlHelp file (*.chm)'
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = edURLChange
        end
        object cbTrimNamespacesOHS: TComboBox
          Left = 94
          Top = 70
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = cbTrimNamespacesOHSChange
        end
      end
      object cbOHSAtTop: TCheckBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 790
        Height = 17
        Align = alTop
        Caption = 'Display other help sources at the top of the result list'
        TabOrder = 2
      end
      object cbCheckGID: TCheckBox
        AlignWithMargins = True
        Left = 5
        Top = 41
        Width = 790
        Height = 17
        Align = alTop
        Caption = 
          'check winhelp sources (*.hlp files) if keyword is valid (help re' +
          'quest may be a bit slower)'
        TabOrder = 3
      end
    end
  end
end
