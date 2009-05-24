object CnAAFontDlg: TCnAAFontDlg
  Left = 172
  Top = 130
  BorderStyle = bsDialog
  Caption = '平滑特效字体'
  ClientHeight = 249
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 8
    Top = 200
    Width = 249
    Height = 43
    AutoSize = True
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 7
  end
  object gbShadow: TGroupBox
    Left = 8
    Top = 8
    Width = 129
    Height = 185
    Caption = '阴影(&S)'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 64
      Width = 42
      Height = 12
      Caption = '模糊度:'
    end
    object Label2: TLabel
      Left = 8
      Top = 96
      Width = 54
      Height = 12
      Caption = '不透明度:'
    end
    object Label3: TLabel
      Left = 8
      Top = 128
      Width = 54
      Height = 12
      Caption = '水平偏移:'
    end
    object Label4: TLabel
      Left = 8
      Top = 160
      Width = 54
      Height = 12
      Caption = '垂直偏移:'
    end
    object spShadow: TShape
      Left = 64
      Top = 37
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label5: TLabel
      Left = 8
      Top = 40
      Width = 54
      Height = 12
      Caption = '阴影颜色:'
    end
    object cbShadow: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = '显示阴影'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object seShadowBlur: TCnSpinEdit
      Left = 64
      Top = 60
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seShadowAlpha: TCnSpinEdit
      Left = 64
      Top = 92
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seOffsetX: TCnSpinEdit
      Left = 64
      Top = 124
      Width = 57
      Height = 21
      MaxLength = 2
      MaxValue = 20
      MinValue = -20
      TabOrder = 3
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seOffsetY: TCnSpinEdit
      Left = 64
      Top = 156
      Width = 57
      Height = 21
      MaxLength = 2
      MaxValue = 20
      MinValue = -20
      TabOrder = 4
      Value = 0
      OnChange = seShadowBlurClick
    end
  end
  object gbGradual: TGroupBox
    Left = 144
    Top = 8
    Width = 113
    Height = 185
    Caption = '渐变(&G)'
    TabOrder = 1
    object spStartColor: TShape
      Left = 64
      Top = 33
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label10: TLabel
      Left = 8
      Top = 36
      Width = 54
      Height = 12
      Caption = '起始颜色:'
    end
    object spEndColor: TShape
      Left = 64
      Top = 54
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label6: TLabel
      Left = 8
      Top = 57
      Width = 54
      Height = 12
      Caption = '结束颜色:'
    end
    object cbGradual: TCheckBox
      Left = 8
      Top = 16
      Width = 100
      Height = 17
      Caption = '允许渐变绘制'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object rbLeftToRight: TRadioButton
      Left = 8
      Top = 75
      Width = 100
      Height = 17
      Caption = '从左到右渐变'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = seShadowBlurClick
    end
    object rbRightToLeft: TRadioButton
      Left = 8
      Top = 92
      Width = 100
      Height = 17
      Caption = '从右到左渐变'
      TabOrder = 2
      OnClick = seShadowBlurClick
    end
    object rbTopToBottom: TRadioButton
      Left = 8
      Top = 109
      Width = 100
      Height = 17
      Caption = '从上到下渐变'
      TabOrder = 3
      OnClick = seShadowBlurClick
    end
    object rbCenterToLR: TRadioButton
      Left = 8
      Top = 142
      Width = 100
      Height = 17
      Caption = '从中间到左右'
      TabOrder = 5
      OnClick = seShadowBlurClick
    end
    object rbBottomToTop: TRadioButton
      Left = 8
      Top = 125
      Width = 100
      Height = 17
      Caption = '从下到上渐变'
      TabOrder = 4
      OnClick = seShadowBlurClick
    end
    object rbCenterToTB: TRadioButton
      Left = 8
      Top = 159
      Width = 100
      Height = 17
      Caption = '从中间到上下'
      TabOrder = 6
      OnClick = seShadowBlurClick
    end
  end
  object gbTexture: TGroupBox
    Left = 264
    Top = 8
    Width = 113
    Height = 185
    Caption = '字体纹理(&T)'
    TabOrder = 2
    object cbTexture: TCheckBox
      Left = 8
      Top = 16
      Width = 100
      Height = 17
      Caption = '允许字体纹理'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object rbTile: TRadioButton
      Left = 8
      Top = 40
      Width = 50
      Height = 17
      Caption = '平铺'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = seShadowBlurClick
    end
    object rbStretched: TRadioButton
      Left = 56
      Top = 40
      Width = 50
      Height = 17
      Caption = '缩放'
      TabOrder = 2
      OnClick = seShadowBlurClick
    end
    object rbCenter: TRadioButton
      Left = 8
      Top = 61
      Width = 50
      Height = 17
      Caption = '中心'
      TabOrder = 3
      OnClick = seShadowBlurClick
    end
    object rbNormal: TRadioButton
      Left = 56
      Top = 61
      Width = 50
      Height = 17
      Caption = '默认'
      TabOrder = 4
      OnClick = seShadowBlurClick
    end
    object btnOpenPic: TButton
      Left = 8
      Top = 104
      Width = 97
      Height = 25
      Caption = '选择图片(&P)...'
      TabOrder = 5
      OnClick = btnOpenPicClick
    end
    object btnClearPic: TButton
      Left = 8
      Top = 144
      Width = 97
      Height = 25
      Caption = '清除图片(&L)'
      TabOrder = 6
      OnClick = btnClearPicClick
    end
  end
  object gbOther: TGroupBox
    Left = 384
    Top = 8
    Width = 129
    Height = 185
    Caption = '其它效果(&O)'
    TabOrder = 3
    object Label7: TLabel
      Left = 8
      Top = 20
      Width = 42
      Height = 12
      Caption = '模糊度:'
    end
    object Label8: TLabel
      Left = 8
      Top = 43
      Width = 54
      Height = 12
      Caption = '不透明度:'
    end
    object Label9: TLabel
      Left = 8
      Top = 89
      Width = 42
      Height = 12
      Caption = '噪声点:'
    end
    object Label11: TLabel
      Left = 8
      Top = 112
      Width = 54
      Height = 12
      Caption = '喷溅效果:'
    end
    object Label12: TLabel
      Left = 8
      Top = 66
      Width = 54
      Height = 12
      Caption = '旋转角度:'
    end
    object cbOutline: TCheckBox
      Left = 8
      Top = 131
      Width = 100
      Height = 17
      Caption = '轮廓效果'
      TabOrder = 5
      OnClick = seShadowBlurClick
    end
    object seBlur: TCnSpinEdit
      Left = 64
      Top = 16
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seAlpha: TCnSpinEdit
      Left = 64
      Top = 39
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seNoise: TCnSpinEdit
      Left = 64
      Top = 85
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seSpray: TCnSpinEdit
      Left = 64
      Top = 108
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 255
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seAngle: TCnSpinEdit
      Left = 64
      Top = 62
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 360
      MinValue = -360
      TabOrder = 2
      Value = 0
      OnChange = seShadowBlurClick
    end
    object cbHorzMirror: TCheckBox
      Left = 8
      Top = 147
      Width = 100
      Height = 17
      Caption = '水平镜象效果'
      TabOrder = 6
      OnClick = seShadowBlurClick
    end
    object cbVertMirror: TCheckBox
      Left = 8
      Top = 163
      Width = 100
      Height = 17
      Caption = '垂直镜象效果'
      TabOrder = 7
      OnClick = seShadowBlurClick
    end
  end
  object btnFont: TButton
    Left = 264
    Top = 216
    Width = 75
    Height = 25
    Caption = '字体(&F)...'
    TabOrder = 4
    OnClick = btnFontClick
  end
  object btnOK: TButton
    Left = 352
    Top = 216
    Width = 75
    Height = 25
    Caption = '确定(&O)'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 432
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = '取消(&C)'
    ModalResult = 2
    TabOrder = 6
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 24
    Top = 208
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 56
    Top = 208
  end
  object ColorDialog: TColorDialog
    Left = 88
    Top = 208
  end
end
