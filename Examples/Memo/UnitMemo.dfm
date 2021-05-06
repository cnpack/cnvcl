object CnMemoForm: TCnMemoForm
  Left = 201
  Top = 197
  Width = 850
  Height = 512
  Caption = 'CnMemo Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 825
    Height = 465
    ActivePage = tsTextControl
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'CnMemo'
      object lblLeftMargin: TLabel
        Left = 344
        Top = 24
        Width = 53
        Height = 13
        Caption = 'Left Margin'
      end
      object lblRightMargin: TLabel
        Left = 480
        Top = 24
        Width = 60
        Height = 13
        Caption = 'Right Margin'
      end
      object chkShowLineNumber: TCheckBox
        Left = 16
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Show LineNumber'
        TabOrder = 0
        OnClick = chkShowLineNumberClick
      end
      object chkHilightLineNumber: TCheckBox
        Left = 152
        Top = 24
        Width = 161
        Height = 17
        Caption = 'Highlight Current LineNumber'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object btnChangeFont: TButton
        Left = 400
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Change Font'
        TabOrder = 2
        OnClick = btnChangeFontClick
      end
      object grpColors: TGroupBox
        Left = 568
        Top = 72
        Width = 185
        Height = 153
        Caption = 'Colors'
        TabOrder = 3
        object btnLineBkColor: TButton
          Left = 16
          Top = 24
          Width = 153
          Height = 25
          Caption = 'Line Number Background'
          TabOrder = 0
        end
        object btnLineNumberColor: TButton
          Left = 16
          Top = 64
          Width = 153
          Height = 25
          Caption = 'Line Number'
          TabOrder = 1
        end
        object btnLineNumberHighlight: TButton
          Left = 16
          Top = 104
          Width = 153
          Height = 25
          Caption = 'Line Number Highlight'
          TabOrder = 2
        end
      end
      object edtMemoLeftMargin: TEdit
        Left = 408
        Top = 24
        Width = 49
        Height = 21
        TabOrder = 4
        Text = '0'
      end
      object edtMemoRightMargin: TEdit
        Left = 552
        Top = 24
        Width = 49
        Height = 21
        TabOrder = 5
        Text = '0'
      end
      object udMemoLeftMargin: TUpDown
        Left = 457
        Top = 24
        Width = 15
        Height = 21
        Associate = edtMemoLeftMargin
        Min = 0
        Position = 0
        TabOrder = 6
        Wrap = False
      end
      object udMemoRightMargin: TUpDown
        Left = 601
        Top = 24
        Width = 15
        Height = 21
        Associate = edtMemoRightMargin
        Min = 0
        Position = 0
        TabOrder = 7
        Wrap = False
      end
      object chkMemoShowCaret: TCheckBox
        Left = 400
        Top = 112
        Width = 113
        Height = 17
        Caption = 'Show Caret'
        TabOrder = 8
        OnClick = chkMemoShowCaretClick
      end
      object chkMemoUseSelection: TCheckBox
        Left = 400
        Top = 160
        Width = 97
        Height = 17
        Caption = 'Use Selection'
        TabOrder = 9
        OnClick = chkMemoUseSelectionClick
      end
      object btnMemoLoad: TButton
        Left = 400
        Top = 72
        Width = 89
        Height = 25
        Caption = 'Load From File'
        TabOrder = 10
        OnClick = btnMemoLoadClick
      end
      object chkCaretAfterLineEnd: TCheckBox
        Left = 400
        Top = 136
        Width = 129
        Height = 17
        Caption = 'Caret After Line End'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = chkCaretAfterLineEndClick
      end
    end
    object tsEditorStringList: TTabSheet
      Caption = 'EditorStringList'
      ImageIndex = 1
      object mmoEditorStringList: TMemo
        Left = 8
        Top = 16
        Width = 353
        Height = 409
        TabOrder = 0
      end
    end
    object tsTextControl: TTabSheet
      Caption = 'TextControl'
      ImageIndex = 2
      object Label1: TLabel
        Left = 400
        Top = 16
        Width = 32
        Height = 13
        Caption = 'Label1'
      end
      object lblCaretRow: TLabel
        Left = 408
        Top = 144
        Width = 53
        Height = 13
        Caption = 'Caret Row:'
      end
      object lblCaretCol: TLabel
        Left = 408
        Top = 176
        Width = 46
        Height = 13
        Caption = 'Caret Col:'
      end
      object chkTCLine: TCheckBox
        Left = 400
        Top = 40
        Width = 113
        Height = 17
        Caption = 'Show LineNumber'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkTCLineClick
      end
      object btnTCFont: TButton
        Left = 408
        Top = 72
        Width = 75
        Height = 25
        Caption = 'Font'
        TabOrder = 1
        OnClick = btnTCFontClick
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 418
        Width = 817
        Height = 19
        Panels = <>
        SimplePanel = False
      end
      object chkShowCaret: TCheckBox
        Left = 400
        Top = 112
        Width = 113
        Height = 17
        Caption = 'Show Caret'
        TabOrder = 3
        OnClick = chkShowCaretClick
      end
      object edtCaretRow: TEdit
        Left = 472
        Top = 144
        Width = 49
        Height = 21
        TabOrder = 4
        Text = '1'
        OnChange = edtCaretRowChange
      end
      object edtCaretCol: TEdit
        Left = 472
        Top = 176
        Width = 49
        Height = 21
        TabOrder = 5
        Text = '1'
        OnChange = edtCaretColChange
      end
      object udCaretRow: TUpDown
        Left = 521
        Top = 144
        Width = 15
        Height = 21
        Associate = edtCaretRow
        Min = 1
        Max = 1000
        Position = 1
        TabOrder = 6
        Wrap = False
      end
      object udCaretCol: TUpDown
        Left = 521
        Top = 176
        Width = 15
        Height = 21
        Associate = edtCaretCol
        Min = 1
        Max = 1000
        Position = 1
        TabOrder = 7
        Wrap = False
      end
      object chkUseSelection: TCheckBox
        Left = 400
        Top = 216
        Width = 97
        Height = 17
        Caption = 'Use Selection'
        TabOrder = 8
        OnClick = chkUseSelectionClick
      end
    end
  end
  object dlgFontMemo: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 720
    Top = 72
  end
  object dlgColor: TColorDialog
    Ctl3D = True
    Left = 376
    Top = 40
  end
  object dlgOpen1: TOpenDialog
    Left = 404
    Top = 240
  end
end
