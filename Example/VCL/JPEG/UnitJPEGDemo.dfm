object FormJPEGDemo: TFormJPEGDemo
  Left = 192
  Top = 107
  Width = 800
  Height = 600
  Caption = 'CnJPEG Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 385
    Top = 41
    Width = 4
    Height = 527
    Cursor = crHSplit
    Align = alRight
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 792
    Height = 41
    Align = alTop
    TabOrder = 0
    object LabelQuality: TLabel
      Left = 310
      Top = 0
      Width = 56
      Height = 13
      Caption = 'Quality: 100'
    end
    object btnLoad: TButton
      Left = 8
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Load JPEG'
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 80
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Save JPEG'
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnConvert: TButton
      Left = 152
      Top = 8
      Width = 75
      Height = 25
      Caption = 'BMP->JPEG'
      TabOrder = 2
      OnClick = btnConvertClick
    end
    object btnCompare: TButton
      Left = 236
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Compare'
      TabOrder = 3
      OnClick = btnCompareClick
    end
    object TrackBarQuality: TTrackBar
      Left = 310
      Top = 8
      Width = 121
      Height = 25
      Max = 100
      Min = 1
      Orientation = trHorizontal
      Frequency = 1
      Position = 100
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBarQualityChange
    end
    object chkGrayscale: TCheckBox
      Left = 440
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 5
      OnClick = chkGrayscaleClick
    end
    object chkProgressive: TCheckBox
      Left = 510
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Progress.'
      TabOrder = 6
      OnClick = chkProgressiveClick
    end
    object chkSmoothing: TCheckBox
      Left = 580
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Smooth'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = chkSmoothingClick
    end
    object cmbScale: TComboBox
      Left = 650
      Top = 8
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
      OnChange = cmbScaleChange
      Items.Strings = (
        'Full'
        '1/2'
        '1/4'
        '1/8')
    end
    object cmbPerformance: TComboBox
      Left = 720
      Top = 8
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      OnChange = cmbPerformanceChange
      Items.Strings = (
        'Quality'
        'Speed')
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 41
    Width = 385
    Height = 527
    Align = alClient
    TabOrder = 1
    object LabelCnJPEG: TLabel
      Left = 1
      Top = 1
      Width = 383
      Height = 13
      Align = alTop
      Caption = 'CnJPEG'
    end
    object ImageCnJPEG: TImage
      Left = 1
      Top = 14
      Width = 383
      Height = 512
      Align = alClient
      Center = True
    end
  end
  object PanelRight: TPanel
    Left = 389
    Top = 41
    Width = 403
    Height = 527
    Align = alRight
    TabOrder = 2
    object LabelNative: TLabel
      Left = 1
      Top = 1
      Width = 401
      Height = 13
      Align = alTop
      Caption = 'Native TJPEGImage'
    end
    object ImageNative: TImage
      Left = 1
      Top = 14
      Width = 401
      Height = 350
      Align = alTop
      Center = True
    end
    object MemoInfo: TMemo
      Left = 1
      Top = 392
      Width = 401
      Height = 134
      Align = alBottom
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 8
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    Left = 40
    Top = 48
  end
end
