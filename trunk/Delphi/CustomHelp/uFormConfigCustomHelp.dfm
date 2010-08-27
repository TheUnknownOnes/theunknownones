object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Custom Help'
  ClientHeight = 773
  ClientWidth = 970
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object Tabs: TPageControl
    Left = 0
    Top = 0
    Width = 868
    Height = 773
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = TabSheet7
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'General'
      object Label1: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 701
        Width = 852
        Height = 36
        Cursor = crHandPoint
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        Caption = 
          'go to the discussion thread at www.delphipraxis.net to receive h' +
          'elp or post provider URLs (German and English spoken)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
        WordWrap = True
        OnClick = Label1Click
        ExplicitWidth = 833
      end
      object rgDisplayLocation: TRadioGroup
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 852
        Height = 238
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Where to display help?'
        TabOrder = 0
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 250
        Width = 852
        Height = 443
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Caption = 'Result order'
        TabOrder = 1
        object lbOrder: TListBox
          AlignWithMargins = True
          Left = 6
          Top = 23
          Width = 840
          Height = 414
          Hint = 'Use drag and drop to rearrange items'
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          DragMode = dmAutomatic
          ExtendedSelect = False
          ItemHeight = 17
          TabOrder = 0
          OnDragDrop = lbOrderDragDrop
          OnDragOver = lbOrderDragOver
        end
      end
    end
    object TabSheet2: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Microsoft Help 2.x'
      ImageIndex = 1
      object lvNamespaces: TListView
        AlignWithMargins = True
        Left = 4
        Top = 41
        Width = 852
        Height = 640
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Help namespace'
            Width = 262
          end
          item
            AutoSize = True
            Caption = 'Description'
          end
          item
            Width = 65
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Panel4: TPanel
        Left = 0
        Top = 685
        Width = 860
        Height = 56
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label4: TLabel
          Left = 8
          Top = 29
          Width = 224
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Trim namespaces until result is found'
        end
        object cbFullTextSearch: TCheckBox
          Left = 8
          Top = 4
          Width = 757
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 
            'Perform fulltext search ... this may be slow (if unchecked only ' +
            'index searches are performed)'
          TabOrder = 0
        end
        object cbTrimNamespacesHX: TComboBox
          Left = 251
          Top = 25
          Width = 190
          Height = 25
          Hint = 
            'Example:'#13#10'Full Keyword is  Classes.TStringList.Create'#13#10'Trim Firs' +
            't searches for TStringList.Create'#13#10'Trim Full searches for Create'
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          ItemHeight = 17
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
      end
      inline fccMSHelp: TFrameConfigColor
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 852
        Height = 29
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = True
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 852
        ExplicitHeight = 29
        inherited catbtnTopics: TCategoryButtons
          Width = 332
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 332
          ExplicitHeight = 29
        end
        inherited ColorBox1: TColorBox
          Left = 340
          Width = 190
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 340
          ExplicitWidth = 190
        end
      end
    end
    object TabSheet3: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Static Web based providers'
      ImageIndex = 2
      inline FrameConfigWebBasedProviders: TFrameConfigProviders
        Left = 0
        Top = 37
        Width = 860
        Height = 704
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 37
        ExplicitWidth = 860
        ExplicitHeight = 704
        inherited pnlOHSItem: TPanel
          Top = 581
          Width = 860
          Height = 123
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitTop = 581
          ExplicitWidth = 860
          ExplicitHeight = 123
          DesignSize = (
            860
            123)
          inherited Label1: TLabel
            Left = 8
            Top = 8
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 8
            ExplicitWidth = 35
            ExplicitHeight = 17
          end
          inherited Label2: TLabel
            Left = 8
            Top = 38
            Width = 68
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 38
            ExplicitWidth = 68
            ExplicitHeight = 17
          end
          inherited LabelURLPath: TLabel
            Left = 8
            Top = 68
            Width = 20
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 68
            ExplicitWidth = 20
            ExplicitHeight = 17
          end
          inherited Label8: TLabel
            Left = 8
            Top = 95
            Width = 106
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 95
            ExplicitWidth = 106
            ExplicitHeight = 17
          end
          inherited BtnBrowseForFile: TSpeedButton
            Left = 819
            Top = 63
            Width = 30
            Height = 29
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 819
            ExplicitTop = 63
            ExplicitWidth = 30
            ExplicitHeight = 29
          end
          inherited edName: TEdit
            Left = 123
            Top = 4
            Width = 730
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 4
            ExplicitWidth = 730
            ExplicitHeight = 25
          end
          inherited edDesc: TEdit
            Left = 123
            Top = 34
            Width = 730
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 34
            ExplicitWidth = 730
            ExplicitHeight = 25
          end
          inherited edURL: TEdit
            Left = 123
            Top = 63
            Width = 694
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 63
            ExplicitWidth = 694
            ExplicitHeight = 25
          end
          inherited cbTrimNamespacesOHS: TComboBox
            Left = 123
            Top = 92
            Width = 190
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ItemHeight = 17
            ExplicitLeft = 123
            ExplicitTop = 92
            ExplicitWidth = 190
            ExplicitHeight = 25
          end
        end
        inherited ListView1: TListView
          Left = 4
          Top = 4
          Width = 852
          Height = 573
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Columns = <
            item
              Caption = 'Name'
              Width = 131
            end
            item
              Caption = 'Description'
              Width = 131
            end
            item
              AutoSize = True
              Caption = '%s ... $(HelpString) will be replaced by Keyword'
            end
            item
              Caption = 'Trim Namespaces'
              Width = 105
            end>
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 852
          ExplicitHeight = 573
        end
      end
      inline fccWebProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 852
        Height = 29
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = True
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 852
        ExplicitHeight = 29
        inherited catbtnTopics: TCategoryButtons
          Width = 332
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 332
          ExplicitHeight = 29
        end
        inherited ColorBox1: TColorBox
          Left = 340
          Width = 190
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 340
          ExplicitWidth = 190
        end
      end
    end
    object TabSheet4: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'File based providers'
      ImageIndex = 3
      inline FrameConfigFileBasedProviders: TFrameConfigProviders
        Left = 0
        Top = 105
        Width = 860
        Height = 636
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 105
        ExplicitWidth = 860
        ExplicitHeight = 636
        inherited pnlOHSItem: TPanel
          Top = 513
          Width = 860
          Height = 123
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitTop = 513
          ExplicitWidth = 860
          ExplicitHeight = 123
          inherited Label1: TLabel
            Left = 8
            Top = 8
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 8
            ExplicitWidth = 35
            ExplicitHeight = 17
          end
          inherited Label2: TLabel
            Left = 8
            Top = 38
            Width = 68
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 38
            ExplicitWidth = 68
            ExplicitHeight = 17
          end
          inherited LabelURLPath: TLabel
            Left = 8
            Top = 68
            Width = 20
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 68
            ExplicitWidth = 20
            ExplicitHeight = 17
          end
          inherited Label8: TLabel
            Left = 8
            Top = 95
            Width = 106
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 95
            ExplicitWidth = 106
            ExplicitHeight = 17
          end
          inherited BtnBrowseForFile: TSpeedButton
            Left = 819
            Top = 63
            Width = 30
            Height = 29
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 819
            ExplicitTop = 63
            ExplicitWidth = 30
            ExplicitHeight = 29
          end
          inherited edName: TEdit
            Left = 123
            Top = 4
            Width = 730
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 4
            ExplicitWidth = 730
            ExplicitHeight = 25
          end
          inherited edDesc: TEdit
            Left = 123
            Top = 34
            Width = 730
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 34
            ExplicitWidth = 730
            ExplicitHeight = 25
          end
          inherited edURL: TEdit
            Left = 123
            Top = 63
            Width = 694
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 63
            ExplicitWidth = 694
            ExplicitHeight = 25
          end
          inherited cbTrimNamespacesOHS: TComboBox
            Left = 123
            Top = 92
            Width = 190
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ItemHeight = 17
            ExplicitLeft = 123
            ExplicitTop = 92
            ExplicitWidth = 190
            ExplicitHeight = 25
          end
        end
        inherited ListView1: TListView
          Left = 4
          Top = 4
          Width = 852
          Height = 505
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Columns = <
            item
              Caption = 'Name'
              Width = 131
            end
            item
              Caption = 'Description'
              Width = 131
            end
            item
              AutoSize = True
              Caption = '%s ... $(HelpString) will be replaced by Keyword'
            end
            item
              Caption = 'Trim Namespaces'
              Width = 105
            end>
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 852
          ExplicitHeight = 505
        end
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 41
        Width = 852
        Height = 60
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'Filetype specific options'
        TabOrder = 1
        object cbCheckGID: TCheckBox
          AlignWithMargins = True
          Left = 6
          Top = 23
          Width = 840
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          Caption = 
            'check winhelp sources (*.hlp files) if keyword is valid (help re' +
            'quest may be a bit slower)'
          TabOrder = 0
        end
      end
      inline fccFileProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 852
        Height = 29
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = True
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 852
        ExplicitHeight = 29
        inherited catbtnTopics: TCategoryButtons
          Width = 332
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 332
          ExplicitHeight = 29
        end
        inherited ColorBox1: TColorBox
          Left = 340
          Width = 190
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 340
          ExplicitWidth = 190
        end
      end
    end
    object TabSheet5: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Dynamic Web based providers (RSS)'
      ImageIndex = 4
      inline FrameConfigRSSProviders: TFrameConfigProviders
        Left = 0
        Top = 37
        Width = 860
        Height = 704
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitTop = 37
        ExplicitWidth = 860
        ExplicitHeight = 704
        inherited pnlOHSItem: TPanel
          Top = 581
          Width = 860
          Height = 123
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitTop = 581
          ExplicitWidth = 860
          ExplicitHeight = 123
          inherited Label1: TLabel
            Left = 8
            Top = 8
            Width = 35
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 8
            ExplicitWidth = 35
            ExplicitHeight = 17
          end
          inherited Label2: TLabel
            Left = 8
            Top = 38
            Width = 68
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 38
            ExplicitWidth = 68
            ExplicitHeight = 17
          end
          inherited LabelURLPath: TLabel
            Left = 8
            Top = 68
            Width = 20
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 68
            ExplicitWidth = 20
            ExplicitHeight = 17
          end
          inherited Label8: TLabel
            Left = 8
            Top = 95
            Width = 106
            Height = 17
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 8
            ExplicitTop = 95
            ExplicitWidth = 106
            ExplicitHeight = 17
          end
          inherited BtnBrowseForFile: TSpeedButton
            Left = 821
            Top = 63
            Width = 30
            Height = 29
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 821
            ExplicitTop = 63
            ExplicitWidth = 30
            ExplicitHeight = 29
          end
          inherited edName: TEdit
            Left = 123
            Top = 4
            Width = 732
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 4
            ExplicitWidth = 732
            ExplicitHeight = 25
          end
          inherited edDesc: TEdit
            Left = 123
            Top = 34
            Width = 732
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 34
            ExplicitWidth = 732
            ExplicitHeight = 25
          end
          inherited edURL: TEdit
            Left = 123
            Top = 63
            Width = 697
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 123
            ExplicitTop = 63
            ExplicitWidth = 697
            ExplicitHeight = 25
          end
          inherited cbTrimNamespacesOHS: TComboBox
            Left = 123
            Top = 92
            Width = 190
            Height = 25
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ItemHeight = 17
            ExplicitLeft = 123
            ExplicitTop = 92
            ExplicitWidth = 190
            ExplicitHeight = 25
          end
        end
        inherited ListView1: TListView
          Left = 4
          Top = 4
          Width = 852
          Height = 573
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Columns = <
            item
              Caption = 'Name'
              Width = 131
            end
            item
              Caption = 'Description'
              Width = 131
            end
            item
              AutoSize = True
              Caption = '%s ... $(HelpString) will be replaced by Keyword'
            end
            item
              Caption = 'Trim Namespaces'
              Width = 105
            end>
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 852
          ExplicitHeight = 573
        end
        inherited OpenDialog1: TOpenDialog
          Left = 600
          Top = 24
        end
      end
      inline fccRSSProvider: TFrameConfigColor
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 852
        Height = 29
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        AutoSize = True
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 852
        ExplicitHeight = 29
        inherited catbtnTopics: TCategoryButtons
          Width = 332
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 332
          ExplicitHeight = 29
        end
        inherited ColorBox1: TColorBox
          Left = 340
          Width = 190
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 340
          ExplicitWidth = 190
        end
      end
    end
    object TabSheet6: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '3rd party providers'
      ImageIndex = 5
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 860
        Height = 741
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object FlowPanel1: TFlowPanel
          Left = 0
          Top = 0
          Width = 860
          Height = 54
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Windows Search'
      ImageIndex = 6
      object ScrollBox2: TScrollBox
        Left = 0
        Top = 57
        Width = 860
        Height = 684
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        ExplicitTop = 29
        ExplicitHeight = 712
        object FlowPanel2: TFlowPanel
          Left = 0
          Top = 0
          Width = 860
          Height = 29
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 860
        Height = 57
        ButtonHeight = 51
        ButtonWidth = 35
        Caption = 'ToolBar1'
        Images = ImageList1
        ShowCaptions = True
        TabOrder = 1
        object ToolButton1: TToolButton
          Left = 0
          Top = 0
          Caption = 'Add'
          ImageIndex = 0
          OnClick = ToolButton1Click
        end
      end
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 872
    Top = 4
    Width = 94
    Height = 765
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 86
      Height = 34
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 4
      Top = 46
      Width = 86
      Height = 34
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ImageList1: TImageList
    Height = 24
    Width = 24
    Left = 480
    Top = 392
    Bitmap = {
      494C010101003000440018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      000000000000000000000000000000000000000000000000000000000000AD7B
      7B00B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400EFDEC600F7E7C600F7DEBD00F7DEB500F7D6B500F7D6AD00F7D6A500EFCE
      9C00EFCE9C00EFCE9400EFCE9400EFCE9400EFCE9400EFCE9400EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400EFDEC600F7E7C600F7DEC600F7DEBD00F7D6B500F7D6AD00F7D6AD00EFCE
      A500EFCE9C00EFCE9C00EFCE9400EFCE9400EFCE9400EFCE9400EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400F7DECE00F7E7CE00F7DEC600F7DEC600F7DEBD00F7D6B500F7D6AD00F7D6
      AD00EFCEA500EFCE9C00EFCE9C00EFCE9400EFCE9400EFCE9400EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400F7E7CE00F7E7D600F7E7CE00F7DEC600F7DEBD00F7DEBD00F7D6B500F7D6
      AD00F7D6AD00EFCEA500EFCE9C00EFCE9C00EFCE9400EFCE9400EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400F7E7D600F7EFDE00F7E7D600F7E7CE00F7DEC600F7DEC600F7DEBD00F7D6
      B500F7D6AD00F7D6AD00EFCEA500EFCE9C00EFCE9C00EFCE9400EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD7B
      8400F7E7DE00F7EFDE00F7E7D600F7E7CE00F7E7CE00F7DEC600F7DEC600F7DE
      BD00F7D6B500F7D6AD00F7D6AD00EFCEA500EFCE9C00EFCE9C00EFCE9400EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B57B
      8400F7EFE700F7EFE700F7EFDE00F7E7D600F7E7CE00F7E7CE00F7DEC600F7DE
      BD00F7DEBD00F7D6B500F7D6AD00F7D6AD00EFCEA500EFCE9C00EFCE9C00EFCE
      9400EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B584
      8400F7EFE700FFF7EF00F7EFE700F7EFDE00F7E7D600F7E7D600F7E7CE00F7DE
      C600F7DEBD00F7DEBD00F7D6B500F7D6AD00F7D6AD00EFCEA500EFCE9C00EFCE
      9C00EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD84
      8400F7EFEF00FFF7EF00F7EFE700F7EFE700F7EFDE00F7E7D600F7E7CE00F7E7
      CE00F7DEC600F7DEBD00F7DEBD00F7D6B500F7D6AD00F7D6AD00EFCEA500EFCE
      9C00EFCE9400B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD84
      8C00FFF7F700FFFFF700FFF7EF00F7EFE700F7EFE700F7EFDE00F7E7D600F7E7
      CE00F7E7CE00F7DEC600F7DEC600F7DEBD00F7D6B500F7D6AD00F7D6AD00EFCE
      A500EFCE9C00B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C68C
      8C00FFF7F700FFFFFF00FFF7F700FFF7EF00F7EFE700F7EFE700F7EFDE00F7E7
      D600F7E7CE00F7E7CE00F7DEC600F7DEBD00F7DEBD00F7D6B500F7D6AD00F7D6
      AD00EFCEA500B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C68C
      8C00FFF7F700FFFFFF00FFFFFF00FFF7F700FFF7EF00F7EFE700F7EFE700F7EF
      DE00F7E7D600F7E7CE00F7E7CE00F7DEC600F7DEBD00F7DEBD00F7D6B500F7D6
      AD00EFCEA500B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C68C
      8C00FFF7F700FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF00F7EFE700F7EF
      E700F7EFDE00F7E7D600F7E7CE00F7E7CE00F7DEC600F7DEBD00F7DEBD00F7D6
      B500F7D6AD00B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE94
      8C00FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF00F7EF
      E700F7EFE700F7EFDE00F7E7D600F7E7CE00F7E7CE00F7DEC600F7DEBD00F7DE
      BD00DEC6A500A57B840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE94
      8C00FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7
      EF00F7EFE700F7EFE700F7EFDE00F7E7D600F7E7CE00F7E7CE00EFD6BD00CEBD
      AD00B5AD94009C7B840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D69C
      9400FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      F700FFF7EF00F7EFE700F7EFDE00F7EFDE00F7EFDE00E7DECE00CEBDAD00BDB5
      A500B5AD9C009C7B840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D69C
      9400FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFF7F700FFF7EF00FFF7E700EFDED600B5847300AD847300AD7B7300AD7B
      7300AD7B7300AD7B730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEA5
      9400FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFF700FFF7F700FFF7EF00E7CEC600B5847300E7B58400E7AD6B00EFA5
      5200EFA53900B584840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEA5
      9400FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFF700E7CEC600B5847300EFC68C00F7BD6B00FFB5
      5200B58484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEA5
      9400FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00B5847300EFC68C00F7BD6B00B584
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEA5
      9400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00E7D6D600B5847300EFC68C00B58484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEA5
      8C00E7CEBD00EFD6BD00EFD6BD00EFCEC600E7CEBD00E7CEBD00E7CEBD00E7CE
      BD00DEC6BD00DEC6BD00DEC6BD00CEADA500B5847300B5848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00E00003000000000000000000E0000300
      0000000000000000E00003000000000000000000E00003000000000000000000
      E00003000000000000000000E00003000000000000000000E000030000000000
      00000000E00003000000000000000000E00003000000000000000000E0000300
      0000000000000000E00003000000000000000000E00003000000000000000000
      E00003000000000000000000E00003000000000000000000E000030000000000
      00000000E00003000000000000000000E00003000000000000000000E0000300
      0000000000000000E00003000000000000000000E00007000000000000000000
      E0000F000000000000000000E0001F000000000000000000E0003F0000000000
      00000000FFFFFF00000000000000000000000000000000000000000000000000
      000000000000}
  end
end
