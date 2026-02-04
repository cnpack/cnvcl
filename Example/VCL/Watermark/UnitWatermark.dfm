object FormWatermark: TFormWatermark
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Watermark Test - NOT Ready'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 971
    Height = 531
    ActivePage = tsRaw
    Align = alClient
    TabOrder = 0
    object tsRaw: TTabSheet
      Caption = 'Raw Implementation'
      object img1: TImage
        Left = 16
        Top = 56
        Width = 513
        Height = 385
        Anchors = [akLeft, akTop, akRight, akBottom]
        Stretch = True
      end
      object btnOpenFile: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Open Image'
        TabOrder = 0
        OnClick = btnOpenFileClick
      end
      object btnEmbed: TButton
        Left = 280
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Embed && Save'
        TabOrder = 1
        OnClick = btnEmbedClick
      end
      object btnExtract: TButton
        Left = 384
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Extract'
        TabOrder = 2
        OnClick = btnExtractClick
      end
      object edtWatermark: TEdit
        Left = 104
        Top = 18
        Width = 169
        Height = 21
        TabOrder = 3
        Text = 'CnPack Watermark'
      end
      object memLog: TMemo
        Left = 544
        Top = 16
        Width = 409
        Height = 425
        Anchors = [akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object tsComponent: TTabSheet
      Caption = 'Component Demo'
      ImageIndex = 1
      object img2: TImage
        Left = 16
        Top = 72
        Width = 513
        Height = 369
        Anchors = [akLeft, akTop, akRight, akBottom]
        Stretch = True
      end
      object lblStrength: TLabel
        Left = 544
        Top = 36
        Width = 40
        Height = 13
        Caption = 'Strength'
      end
      object gbEmbed: TGroupBox
        Left = 16
        Top = 8
        Width = 513
        Height = 57
        Caption = '1. Embed Watermark'
        TabOrder = 0
        object btnCompLoad: TButton
          Left = 8
          Top = 20
          Width = 75
          Height = 25
          Caption = 'Open Src'
          TabOrder = 0
          OnClick = btnCompLoadClick
        end
        object edtCompText: TEdit
          Left = 96
          Top = 22
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'CnPack Component'
        end
        object btnCompEmbed: TButton
          Left = 224
          Top = 20
          Width = 97
          Height = 25
          Caption = 'Embed && Save...'
          TabOrder = 2
          OnClick = btnCompEmbedClick
        end
        object btnLoadWatermark: TButton
          Left = 336
          Top = 20
          Width = 105
          Height = 25
          Caption = 'Load Watermark Img'
          TabOrder = 3
          OnClick = btnLoadWatermarkClick
        end
        object rbTextMode: TRadioButton
          Left = 448
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Text'
          Checked = True
          TabOrder = 4
          TabStop = True
        end
        object rbImageMode: TRadioButton
          Left = 448
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Image'
          TabOrder = 5
        end
      end
      object gbExtract: TGroupBox
        Left = 544
        Top = 8
        Width = 409
        Height = 57
        Anchors = [akTop, akRight]
        Caption = '2. Extract / Verify'
        TabOrder = 1
        object btnCompExtract: TButton
          Left = 248
          Top = 20
          Width = 75
          Height = 25
          Caption = 'Extract'
          TabOrder = 0
          OnClick = btnCompExtractClick
        end
        object btnCompVerify: TButton
          Left = 328
          Top = 20
          Width = 65
          Height = 25
          Caption = 'Verify'
          TabOrder = 1
          OnClick = btnCompVerifyClick
        end
        object btnLoadExtractImg: TButton
          Left = 8
          Top = 20
          Width = 89
          Height = 25
          Caption = 'Open Target'
          TabOrder = 2
          OnClick = btnLoadExtractImgClick
        end
      end
      object memCompLog: TMemo
        Left = 544
        Top = 72
        Width = 409
        Height = 369
        Anchors = [akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object pbProgress: TProgressBar
        Left = 544
        Top = 448
        Width = 409
        Height = 17
        Anchors = [akRight, akBottom]
        Min = 0
        Max = 100
        TabOrder = 3
      end
      object cbbStrength: TComboBox
        Left = 664
        Top = 30
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'Low'
          'Medium'
          'High')
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Filter = 'JPG|*.jpg'
    Left = 96
    Top = 16
  end
end
