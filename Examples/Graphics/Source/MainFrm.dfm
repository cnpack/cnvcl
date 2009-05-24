object MainForm: TMainForm
  Left = 74
  Top = 5
  Width = 581
  Height = 448
  Caption = 'CnPack图像处理库演示程序 V0.11Alpha'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 185
    Top = 0
    Width = 388
    Height = 421
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 421
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 1
    object PageControl: TPageControl
      Left = 8
      Top = 8
      Width = 169
      Height = 405
      ActivePage = TabSheet2
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = UpdateImage
      object TabSheet1: TTabSheet
        Caption = '图像'
        object spBackTranColor: TShape
          Left = 136
          Top = 109
          Width = 17
          Height = 17
          OnMouseUp = spColorMouseUp
        end
        object Label1: TLabel
          Left = 8
          Top = 47
          Width = 78
          Height = 12
          Caption = '图片显示方式:'
        end
        object spBackColor: TShape
          Left = 136
          Top = 27
          Width = 17
          Height = 17
          OnMouseUp = spColorMouseUp
        end
        object Label33: TLabel
          Left = 40
          Top = 288
          Width = 78
          Height = 12
          Caption = '(C)2001,2002 '
          Font.Charset = GB2312_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object Label34: TLabel
          Left = 32
          Top = 256
          Width = 84
          Height = 12
          Caption = ' zjy@cnpack.org'
          Font.Charset = GB2312_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object Label35: TLabel
          Left = 16
          Top = 240
          Width = 126
          Height = 12
          Caption = 'http://www.cnpack.org'
          Font.Charset = GB2312_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object Label36: TLabel
          Left = 56
          Top = 272
          Width = 42
          Height = 12
          Caption = '2002.02'
          Font.Charset = GB2312_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object Bevel1: TBevel
          Left = 8
          Top = 224
          Width = 145
          Height = 9
          Shape = bsBottomLine
        end
        object Label37: TLabel
          Left = 24
          Top = 304
          Width = 120
          Height = 12
          Caption = 'CnPack开发组版权所有'
          Font.Charset = GB2312_CHARSET
          Font.Color = clBlue
          Font.Height = -12
          Font.Name = '宋体'
          Font.Style = []
          ParentFont = False
        end
        object cbBackSmooth: TCheckBox
          Left = 8
          Top = 131
          Width = 145
          Height = 17
          Caption = '使用抗锯齿算法'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = cbBackSmoothClick
        end
        object cbBackTran: TCheckBox
          Left = 8
          Top = 89
          Width = 121
          Height = 17
          Caption = '允许背景图透明'
          TabOrder = 3
          OnClick = UpdateImage
        end
        object cbBackTranColor: TCheckBox
          Left = 8
          Top = 109
          Width = 105
          Height = 17
          Caption = '指定透明色'
          TabOrder = 4
          OnClick = UpdateImage
        end
        object cbbBackMode: TComboBox
          Left = 8
          Top = 63
          Width = 145
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 2
          OnClick = UpdateImage
          Items.Strings = (
            'dmDraw'
            'dmCenter'
            'dmStretched'
            'dmResize'
            'dmTiled　')
        end
        object btnBack: TButton
          Left = 47
          Top = 200
          Width = 58
          Height = 23
          Caption = '打开图片'
          TabOrder = 8
          OnClick = btnBackClick
        end
        object rbBackBW: TRadioButton
          Left = 8
          Top = 8
          Width = 113
          Height = 17
          Caption = '使用棋盘背景'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = UpdateImage
        end
        object rbBackColor: TRadioButton
          Left = 8
          Top = 27
          Width = 113
          Height = 17
          Caption = '使用固定色背景'
          TabOrder = 1
          OnClick = UpdateImage
        end
        object sbBackAlpha: TScrollBar
          Left = 8
          Top = 176
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 200
          TabOrder = 7
          OnChange = UpdateImage
        end
        object cbBackAlpha: TCheckBox
          Left = 8
          Top = 153
          Width = 105
          Height = 17
          Caption = '图像半透明效果'
          TabOrder = 6
          OnClick = UpdateImage
        end
      end
      object TabSheet2: TTabSheet
        Caption = '属性'
        ImageIndex = 1
        object Label3: TLabel
          Left = 8
          Top = 9
          Width = 54
          Height = 12
          Caption = '红色分量:'
        end
        object Label4: TLabel
          Left = 8
          Top = 48
          Width = 54
          Height = 12
          Caption = '绿色分量:'
        end
        object Label5: TLabel
          Left = 8
          Top = 87
          Width = 54
          Height = 12
          Caption = '蓝色分量:'
        end
        object Label6: TLabel
          Left = 8
          Top = 144
          Width = 30
          Height = 12
          Caption = '亮度:'
        end
        object Label7: TLabel
          Left = 8
          Top = 182
          Width = 42
          Height = 12
          Caption = '对比度:'
        end
        object Label8: TLabel
          Left = 8
          Top = 221
          Width = 42
          Height = 12
          Caption = '饱和度:'
        end
        object spColor: TShape
          Left = 136
          Top = 277
          Width = 17
          Height = 17
          Brush.Color = 16744576
          OnMouseUp = spColorMouseUp
        end
        object Label23: TLabel
          Left = 8
          Top = 128
          Width = 54
          Height = 12
          Caption = '颜色通道:'
        end
        object sbRed: TScrollBar
          Left = 8
          Top = 24
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 0
          OnChange = UpdateImage
        end
        object sbGreen: TScrollBar
          Left = 8
          Top = 63
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 1
          OnChange = UpdateImage
        end
        object sbBlue: TScrollBar
          Left = 8
          Top = 102
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 2
          OnChange = UpdateImage
        end
        object sbBrightness: TScrollBar
          Left = 8
          Top = 159
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 6
          OnChange = UpdateImage
        end
        object sbContrast: TScrollBar
          Left = 8
          Top = 198
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 7
          OnChange = UpdateImage
        end
        object sbSaturation: TScrollBar
          Left = 8
          Top = 237
          Width = 145
          Height = 16
          Min = -100
          PageSize = 0
          TabOrder = 8
          OnChange = UpdateImage
        end
        object cbGrayscale: TCheckBox
          Left = 8
          Top = 260
          Width = 73
          Height = 17
          Caption = '灰度化'
          TabOrder = 9
          OnClick = UpdateImage
        end
        object cbInvert: TCheckBox
          Left = 88
          Top = 257
          Width = 73
          Height = 17
          Caption = '颜色反转'
          TabOrder = 10
          OnClick = UpdateImage
        end
        object cbColorize: TCheckBox
          Left = 8
          Top = 280
          Width = 121
          Height = 17
          Caption = '按指定颜色彩色化'
          TabOrder = 11
          OnClick = UpdateImage
        end
        object btnBackReset: TButton
          Left = 48
          Top = 299
          Width = 75
          Height = 25
          Caption = '全部复位'
          TabOrder = 12
          OnClick = btnBackResetClick
        end
        object cbRed: TCheckBox
          Tag = 1
          Left = 64
          Top = 125
          Width = 33
          Height = 17
          Caption = '红'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = UpdateImage
        end
        object cbGreen: TCheckBox
          Tag = 1
          Left = 96
          Top = 125
          Width = 33
          Height = 17
          Caption = '绿'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = UpdateImage
        end
        object cbBlue: TCheckBox
          Tag = 1
          Left = 128
          Top = 125
          Width = 33
          Height = 17
          Caption = '蓝'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = UpdateImage
        end
      end
      object TabSheet3: TTabSheet
        Caption = '渐变'
        ImageIndex = 2
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 66
          Height = 12
          Caption = '渐变起始色:'
        end
        object Label9: TLabel
          Left = 8
          Top = 32
          Width = 66
          Height = 12
          Caption = '渐变结束色:'
        end
        object Label10: TLabel
          Left = 8
          Top = 56
          Width = 54
          Height = 12
          Caption = '渐变方式:'
        end
        object spStartColor: TShape
          Left = 88
          Top = 5
          Width = 17
          Height = 17
          Brush.Color = clLime
          OnMouseUp = spColorMouseUp
        end
        object spEndColor: TShape
          Left = 88
          Top = 29
          Width = 17
          Height = 17
          Brush.Color = 16744576
          OnMouseUp = spColorMouseUp
        end
        object Label11: TLabel
          Left = 8
          Top = 104
          Width = 54
          Height = 12
          Caption = '不透明度:'
        end
        object Label12: TLabel
          Left = 8
          Top = 152
          Width = 42
          Height = 12
          Caption = '中间色:'
        end
        object spMiddleColor: TShape
          Left = 56
          Top = 149
          Width = 17
          Height = 17
          Brush.Color = clRed
          OnMouseUp = spColorMouseUp
        end
        object Label13: TLabel
          Left = 78
          Top = 152
          Width = 30
          Height = 12
          Caption = '位置:'
        end
        object cbbGrad: TComboBox
          Left = 8
          Top = 72
          Width = 145
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 0
          OnChange = UpdateImage
          Items.Strings = (
            '从左到右渐变'
            '从右到左渐变'
            '从上到下渐变'
            '从下到上渐变'
            '从中间到两边渐变'
            '从中间到上下渐变'
            '从中间向四周辐射')
        end
        object sbGradAlpha: TScrollBar
          Left = 8
          Top = 120
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 180
          TabOrder = 1
          OnChange = UpdateImage
        end
        object lbGradColor: TListBox
          Left = 8
          Top = 208
          Width = 145
          Height = 105
          ItemHeight = 12
          Style = lbOwnerDrawFixed
          TabOrder = 6
        end
        object seGradPos: TSpinEdit
          Left = 112
          Top = 148
          Width = 41
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          Value = 50
        end
        object btnGradAdd: TBitBtn
          Left = 11
          Top = 176
          Width = 41
          Height = 25
          Caption = '增加'
          TabOrder = 3
          OnClick = btnGradAddClick
        end
        object btnGradDel: TBitBtn
          Left = 59
          Top = 176
          Width = 41
          Height = 25
          Caption = '删除'
          TabOrder = 4
          OnClick = btnGradDelClick
        end
        object btnGradClear: TBitBtn
          Left = 107
          Top = 176
          Width = 41
          Height = 25
          Caption = '清空'
          TabOrder = 5
          OnClick = btnGradClearClick
        end
      end
      object TabSheet4: TTabSheet
        Caption = '滤镜一'
        ImageIndex = 3
        object Label14: TLabel
          Left = 8
          Top = 240
          Width = 42
          Height = 12
          Caption = '参数一:'
        end
        object Label15: TLabel
          Left = 8
          Top = 280
          Width = 42
          Height = 12
          Caption = '参数二:'
        end
        object sbFilter1: TScrollBar
          Left = 8
          Top = 256
          Width = 145
          Height = 16
          Max = 20
          PageSize = 0
          Position = 3
          TabOrder = 1
          OnChange = UpdateImage
        end
        object rgFilterOne: TRadioGroup
          Left = 8
          Top = 0
          Width = 145
          Height = 233
          Caption = '滤镜效果'
          ItemIndex = 0
          Items.Strings = (
            '原始图像'
            '模糊'
            '高斯模糊(参数1)'
            '锐化'
            '更多锐化(参数1)'
            '喷溅效果(参数1)'
            '浮雕效果'
            '马赛克效果(参数2)'
            '旋涡效果(参数1)'
            '彩色噪声点(参数1)'
            '黑白噪声点(参数1)'
            '图像色阶调整(参数2)'
            '壁画效果(参数1)'
            '地图效果(参数1)'
            '水滴效果(参数2)')
          TabOrder = 0
          OnClick = UpdateImage
        end
        object sbFilter2: TScrollBar
          Left = 8
          Top = 296
          Width = 145
          Height = 16
          Max = 20
          PageSize = 0
          Position = 3
          TabOrder = 2
          OnChange = UpdateImage
        end
      end
      object TabSheet5: TTabSheet
        Caption = '滤镜二'
        ImageIndex = 4
        object Label16: TLabel
          Left = 8
          Top = 168
          Width = 60
          Height = 12
          Caption = 'X方向系数:'
        end
        object Label17: TLabel
          Left = 8
          Top = 208
          Width = 60
          Height = 12
          Caption = 'Y方向系数:'
        end
        object Label18: TLabel
          Left = 8
          Top = 248
          Width = 54
          Height = 12
          Caption = '扭曲程度:'
        end
        object seCore00: TSpinEdit
          Left = 8
          Top = 32
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 0
          Value = 1
        end
        object seCore01: TSpinEdit
          Left = 56
          Top = 32
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 1
          Value = 1
        end
        object seCore02: TSpinEdit
          Left = 104
          Top = 32
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 2
          Value = 1
        end
        object seCore10: TSpinEdit
          Left = 8
          Top = 56
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 3
          Value = 1
        end
        object seCore11: TSpinEdit
          Left = 56
          Top = 56
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 4
          Value = 1
        end
        object seCore12: TSpinEdit
          Left = 104
          Top = 56
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 5
          Value = 1
        end
        object seCore20: TSpinEdit
          Left = 8
          Top = 80
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 6
          Value = 1
        end
        object seCore21: TSpinEdit
          Left = 56
          Top = 80
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 7
          Value = 1
        end
        object seCore22: TSpinEdit
          Left = 104
          Top = 80
          Width = 49
          Height = 21
          MaxLength = 3
          MaxValue = 100
          MinValue = -100
          TabOrder = 8
          Value = 1
        end
        object rbDefine: TRadioButton
          Left = 8
          Top = 8
          Width = 145
          Height = 17
          Caption = '自定义滤镜处理算子:'
          Checked = True
          TabOrder = 9
          TabStop = True
          OnClick = UpdateImage
        end
        object rbWave: TRadioButton
          Left = 8
          Top = 144
          Width = 113
          Height = 17
          Caption = '扭曲变形效果:'
          TabOrder = 10
          OnClick = UpdateImage
        end
        object sbWave1: TScrollBar
          Left = 8
          Top = 184
          Width = 145
          Height = 16
          Max = 50
          PageSize = 0
          Position = 10
          TabOrder = 11
          OnChange = UpdateImage
        end
        object sbWave2: TScrollBar
          Left = 8
          Top = 224
          Width = 145
          Height = 16
          Max = 50
          PageSize = 0
          Position = 10
          TabOrder = 12
          OnChange = UpdateImage
        end
        object sbWave3: TScrollBar
          Left = 8
          Top = 264
          Width = 145
          Height = 16
          Max = 50
          PageSize = 0
          Position = 10
          TabOrder = 13
          OnChange = UpdateImage
        end
        object cbWave: TCheckBox
          Left = 8
          Top = 288
          Width = 97
          Height = 17
          Caption = '允许环绕'
          TabOrder = 14
          OnClick = UpdateImage
        end
        object btnDefine: TButton
          Left = 40
          Top = 112
          Width = 75
          Height = 25
          Caption = '刷新显示'
          TabOrder = 15
          OnClick = UpdateImage
        end
      end
      object TabSheet6: TTabSheet
        Caption = '平滑字'
        ImageIndex = 5
        object Label19: TLabel
          Left = 8
          Top = 8
          Width = 30
          Height = 12
          Caption = '文本:'
        end
        object Label20: TLabel
          Left = 8
          Top = 32
          Width = 30
          Height = 12
          Caption = '字体:'
        end
        object spFontColor: TShape
          Left = 96
          Top = 32
          Width = 17
          Height = 17
          Brush.Color = 16744576
          OnMouseUp = spColorMouseUp
        end
        object Label21: TLabel
          Left = 16
          Top = 162
          Width = 12
          Height = 12
          Caption = 'X:'
        end
        object Label22: TLabel
          Left = 88
          Top = 163
          Width = 12
          Height = 12
          Caption = 'Y:'
        end
        object Label24: TLabel
          Left = 8
          Top = 97
          Width = 54
          Height = 12
          Caption = '不透明度:'
        end
        object Label25: TLabel
          Left = 8
          Top = 58
          Width = 30
          Height = 12
          Caption = '精度:'
        end
        object spFontBkColor: TShape
          Left = 133
          Top = 78
          Width = 17
          Height = 17
          Brush.Color = 16744703
          OnMouseUp = spColorMouseUp
        end
        object spFontShadow: TShape
          Left = 85
          Top = 136
          Width = 17
          Height = 17
          Brush.Color = clBlack
          OnMouseUp = spColorMouseUp
        end
        object edtFont: TEdit
          Left = 40
          Top = 5
          Width = 112
          Height = 20
          TabOrder = 0
          Text = 'CnPack图像处理库'
          OnChange = UpdateImage
        end
        object btnFont: TButton
          Left = 120
          Top = 27
          Width = 33
          Height = 25
          Caption = '字体'
          TabOrder = 2
          OnClick = btnFontClick
        end
        object seFontSize: TSpinEdit
          Left = 40
          Top = 29
          Width = 49
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 30
          OnChange = UpdateImage
        end
        object cbFontShadow: TCheckBox
          Left = 8
          Top = 137
          Width = 73
          Height = 17
          Caption = '阴影效果'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = UpdateImage
        end
        object seFontX: TSpinEdit
          Left = 40
          Top = 158
          Width = 41
          Height = 21
          MaxLength = 2
          MaxValue = 20
          MinValue = -20
          TabOrder = 6
          Value = 2
          OnChange = UpdateImage
        end
        object seFontY: TSpinEdit
          Left = 112
          Top = 158
          Width = 41
          Height = 21
          MaxLength = 2
          MaxValue = 20
          MinValue = -20
          TabOrder = 7
          Value = 2
          OnChange = UpdateImage
        end
        object cbFontGrad: TCheckBox
          Left = 8
          Top = 207
          Width = 153
          Height = 17
          Caption = '使用“渐变”页效果渐变'
          Checked = True
          State = cbChecked
          TabOrder = 10
          OnClick = UpdateImage
        end
        object cbFontText: TCheckBox
          Left = 8
          Top = 245
          Width = 97
          Height = 17
          Caption = '纹理效果'
          TabOrder = 12
          OnClick = UpdateImage
        end
        object btnFontText: TButton
          Left = 104
          Top = 244
          Width = 49
          Height = 25
          Caption = '纹理...'
          TabOrder = 13
          OnClick = btnFontTextClick
        end
        object cbFontOutline: TCheckBox
          Left = 8
          Top = 264
          Width = 97
          Height = 17
          Caption = '轮廓效果'
          TabOrder = 14
          OnClick = UpdateImage
        end
        object cbFontNoise: TCheckBox
          Left = 8
          Top = 283
          Width = 97
          Height = 17
          Caption = '噪声效果'
          TabOrder = 15
          OnClick = UpdateImage
        end
        object sbFontNoise: TScrollBar
          Left = 8
          Top = 304
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 40
          TabOrder = 16
          OnChange = UpdateImage
        end
        object sbFontAlpha: TScrollBar
          Left = 8
          Top = 113
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 200
          TabOrder = 3
          OnChange = UpdateImage
        end
        object cbbFont: TComboBox
          Left = 40
          Top = 54
          Width = 112
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 17
          OnChange = UpdateImage
          Items.Strings = (
            '4X4高精度'
            '3X3普通精度'
            '2X2低精度'
            '无平滑效果')
        end
        object cbFontClear: TCheckBox
          Left = 8
          Top = 79
          Width = 97
          Height = 17
          Caption = '填充文本背景'
          TabOrder = 18
          OnClick = UpdateImage
        end
        object cbFontLight: TCheckBox
          Left = 8
          Top = 226
          Width = 153
          Height = 17
          Caption = '使用“光照”页效果光照'
          TabOrder = 11
          OnClick = UpdateImage
        end
        object seFontShadow: TSpinEdit
          Left = 112
          Top = 134
          Width = 41
          Height = 21
          MaxLength = 2
          MaxValue = 20
          MinValue = 0
          TabOrder = 5
          Value = 1
          OnChange = UpdateImage
        end
        object cbFontSpray: TCheckBox
          Left = 8
          Top = 185
          Width = 73
          Height = 17
          Caption = '喷溅效果'
          TabOrder = 8
          OnClick = UpdateImage
        end
        object seFontSpray: TSpinEdit
          Left = 112
          Top = 182
          Width = 41
          Height = 21
          MaxLength = 2
          MaxValue = 20
          MinValue = 0
          TabOrder = 9
          Value = 2
          OnChange = UpdateImage
        end
      end
      object TabSheet7: TTabSheet
        Caption = '图形绘制'
        ImageIndex = 6
        object Label26: TLabel
          Left = 8
          Top = 12
          Width = 54
          Height = 12
          Caption = '画笔粗细:'
        end
        object Label27: TLabel
          Left = 8
          Top = 240
          Width = 42
          Height = 12
          Caption = '参数一:'
        end
        object Label28: TLabel
          Left = 8
          Top = 280
          Width = 42
          Height = 12
          Caption = '参数二:'
        end
        object Label29: TLabel
          Left = 8
          Top = 40
          Width = 54
          Height = 12
          Caption = '画笔颜色:'
        end
        object spPen: TShape
          Left = 64
          Top = 37
          Width = 17
          Height = 17
          Brush.Color = clBlue
          OnMouseUp = spColorMouseUp
        end
        object cbbPen: TComboBox
          Left = 64
          Top = 8
          Width = 89
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 0
          OnChange = UpdateImage
          Items.Strings = (
            '细画笔'
            '普通画笔'
            '粗画笔')
        end
        object rgPen: TRadioGroup
          Left = 8
          Top = 88
          Width = 145
          Height = 145
          Caption = '几何图形'
          ItemIndex = 0
          Items.Strings = (
            '直线'
            '矩形'
            '椭圆'
            '随机折线'
            '抛物线'
            '正弦曲线')
          TabOrder = 2
          OnClick = UpdateImage
        end
        object sbPen1: TScrollBar
          Left = 8
          Top = 256
          Width = 145
          Height = 16
          Max = 90
          Min = 1
          PageSize = 0
          Position = 30
          TabOrder = 3
          OnChange = UpdateImage
        end
        object sbPen2: TScrollBar
          Left = 8
          Top = 296
          Width = 145
          Height = 16
          Max = 90
          Min = 1
          PageSize = 0
          Position = 30
          TabOrder = 4
          OnChange = UpdateImage
        end
        object cbPenSmooth: TCheckBox
          Left = 8
          Top = 64
          Width = 121
          Height = 17
          Caption = '使用抗锯齿算法'
          TabOrder = 1
          OnClick = UpdateImage
        end
      end
      object TabSheet8: TTabSheet
        Caption = '变换'
        ImageIndex = 7
        object Label30: TLabel
          Left = 8
          Top = 176
          Width = 60
          Height = 12
          Caption = '中心位置X:'
        end
        object Label31: TLabel
          Left = 8
          Top = 216
          Width = 60
          Height = 12
          Caption = '中心位置Y:'
        end
        object Label32: TLabel
          Left = 8
          Top = 256
          Width = 54
          Height = 12
          Caption = '旋转角度:'
        end
        object cbFlip: TCheckBox
          Left = 8
          Top = 8
          Width = 97
          Height = 17
          Caption = '水平翻转'
          TabOrder = 0
          OnClick = UpdateImage
        end
        object cbFlop: TCheckBox
          Left = 8
          Top = 32
          Width = 97
          Height = 17
          Caption = '垂直翻转'
          TabOrder = 1
          OnClick = UpdateImage
        end
        object sbVShift: TScrollBar
          Left = 8
          Top = 80
          Width = 145
          Height = 16
          PageSize = 0
          Position = 40
          TabOrder = 2
          OnChange = UpdateImage
        end
        object cbVShift: TCheckBox
          Left = 8
          Top = 56
          Width = 97
          Height = 17
          Caption = '水平平移'
          TabOrder = 3
          OnClick = UpdateImage
        end
        object cbHShift: TCheckBox
          Left = 8
          Top = 104
          Width = 97
          Height = 17
          Caption = '垂直平移'
          TabOrder = 4
          OnClick = UpdateImage
        end
        object sbHShift: TScrollBar
          Left = 8
          Top = 128
          Width = 145
          Height = 16
          PageSize = 0
          Position = 40
          TabOrder = 5
          OnChange = UpdateImage
        end
        object cbRotate: TCheckBox
          Left = 8
          Top = 152
          Width = 97
          Height = 17
          Caption = '旋转位图'
          TabOrder = 6
          OnClick = UpdateImage
        end
        object sbRotateX: TScrollBar
          Left = 8
          Top = 192
          Width = 145
          Height = 16
          Max = 120
          Min = -20
          PageSize = 0
          Position = 50
          TabOrder = 7
          OnChange = UpdateImage
        end
        object sbRotateY: TScrollBar
          Left = 8
          Top = 232
          Width = 145
          Height = 16
          Max = 120
          Min = -20
          PageSize = 0
          Position = 50
          TabOrder = 8
          OnChange = UpdateImage
        end
        object sbRotateAngle: TScrollBar
          Left = 8
          Top = 272
          Width = 145
          Height = 16
          Max = 360
          PageSize = 0
          Position = 30
          TabOrder = 9
          OnChange = UpdateImage
        end
        object cbRotateSmooth: TCheckBox
          Left = 8
          Top = 296
          Width = 113
          Height = 17
          Caption = '使用抗锯齿算法'
          Checked = True
          State = cbChecked
          TabOrder = 10
          OnClick = cbBackSmoothClick
        end
      end
      object TabSheet9: TTabSheet
        Caption = '混合'
        ImageIndex = 8
        object Label38: TLabel
          Left = 8
          Top = 32
          Width = 78
          Height = 12
          Caption = '起始不透明度:'
        end
        object Label39: TLabel
          Left = 8
          Top = 72
          Width = 78
          Height = 12
          Caption = '结束不透明度:'
        end
        object Label41: TLabel
          Left = 8
          Top = 112
          Width = 54
          Height = 12
          Caption = '渐变方式:'
        end
        object Label40: TLabel
          Left = 8
          Top = 179
          Width = 78
          Height = 12
          Caption = '目标矩形宽度:'
        end
        object Label42: TLabel
          Left = 8
          Top = 219
          Width = 78
          Height = 12
          Caption = '目标矩形高度:'
        end
        object Label43: TLabel
          Left = 8
          Top = 259
          Width = 78
          Height = 12
          Caption = '前景不透明度:'
        end
        object btnBlend: TButton
          Left = 47
          Top = 299
          Width = 58
          Height = 23
          Caption = '打开图片'
          TabOrder = 3
          OnClick = btnBlendClick
        end
        object sbAlphaStart: TScrollBar
          Left = 8
          Top = 48
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          TabOrder = 0
          OnChange = UpdateImage
        end
        object sbAlphaEnd: TScrollBar
          Left = 8
          Top = 88
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 255
          TabOrder = 1
          OnChange = UpdateImage
        end
        object cbbGradBlend: TComboBox
          Left = 8
          Top = 128
          Width = 145
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 2
          OnChange = UpdateImage
          Items.Strings = (
            '从左到右渐变'
            '从右到左渐变'
            '从上到下渐变'
            '从下到上渐变'
            '从中间到两边渐变'
            '从中间到上下渐变'
            '从中间向四周辐射')
        end
        object rbGradBlend: TRadioButton
          Left = 8
          Top = 8
          Width = 113
          Height = 17
          Caption = '渐变透明的混合'
          Checked = True
          TabOrder = 4
          TabStop = True
          OnClick = UpdateImage
        end
        object rbBlendFore: TRadioButton
          Left = 8
          Top = 155
          Width = 145
          Height = 17
          Caption = '前景背景缩放混合'
          TabOrder = 5
          OnClick = UpdateImage
        end
        object sbDstX: TScrollBar
          Left = 8
          Top = 195
          Width = 145
          Height = 16
          Max = 90
          Min = 5
          PageSize = 0
          Position = 60
          TabOrder = 6
          OnChange = UpdateImage
        end
        object sbDstY: TScrollBar
          Left = 8
          Top = 235
          Width = 145
          Height = 16
          Max = 90
          Min = 5
          PageSize = 0
          Position = 60
          TabOrder = 7
          OnChange = UpdateImage
        end
        object sbForeAlpha: TScrollBar
          Left = 8
          Top = 275
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 150
          TabOrder = 8
          OnChange = UpdateImage
        end
      end
      object TabSheet10: TTabSheet
        Caption = '光照'
        ImageIndex = 9
        object Label44: TLabel
          Left = 8
          Top = 56
          Width = 60
          Height = 12
          Caption = '中心位置X:'
        end
        object Label45: TLabel
          Left = 8
          Top = 96
          Width = 60
          Height = 12
          Caption = '中心位置Y:'
        end
        object Label46: TLabel
          Left = 8
          Top = 136
          Width = 78
          Height = 12
          Caption = '光照范围宽度:'
        end
        object Label47: TLabel
          Left = 8
          Top = 176
          Width = 78
          Height = 12
          Caption = '光照范围高度:'
        end
        object Label48: TLabel
          Left = 8
          Top = 216
          Width = 54
          Height = 12
          Caption = '旋转角度:'
        end
        object Label49: TLabel
          Left = 8
          Top = 256
          Width = 54
          Height = 12
          Caption = '不透明度:'
        end
        object Label50: TLabel
          Left = 8
          Top = 11
          Width = 54
          Height = 12
          Caption = '灯光颜色:'
        end
        object spLight: TShape
          Left = 72
          Top = 8
          Width = 17
          Height = 17
          Brush.Color = clYellow
          OnMouseUp = spColorMouseUp
        end
        object sbLightX: TScrollBar
          Left = 8
          Top = 72
          Width = 145
          Height = 16
          Max = 150
          Min = -50
          PageSize = 0
          Position = -30
          TabOrder = 1
          OnChange = UpdateImage
        end
        object sbLightY: TScrollBar
          Left = 8
          Top = 112
          Width = 145
          Height = 16
          Max = 150
          Min = -50
          PageSize = 0
          Position = -30
          TabOrder = 2
          OnChange = UpdateImage
        end
        object sbLightW: TScrollBar
          Left = 8
          Top = 152
          Width = 145
          Height = 16
          Max = 150
          PageSize = 0
          Position = 50
          TabOrder = 3
          OnChange = UpdateImage
        end
        object sbLightH: TScrollBar
          Left = 8
          Top = 192
          Width = 145
          Height = 16
          Max = 150
          PageSize = 0
          Position = 25
          TabOrder = 4
          OnChange = UpdateImage
        end
        object sbLightAngle: TScrollBar
          Left = 8
          Top = 232
          Width = 145
          Height = 16
          Max = 360
          PageSize = 0
          Position = 30
          TabOrder = 5
          OnChange = UpdateImage
        end
        object sbLightAlpha: TScrollBar
          Left = 8
          Top = 272
          Width = 145
          Height = 16
          Max = 255
          PageSize = 0
          Position = 200
          TabOrder = 6
          OnChange = UpdateImage
        end
        object cbLightBack: TCheckBox
          Left = 8
          Top = 32
          Width = 97
          Height = 17
          Caption = '显示背景图'
          TabOrder = 0
          OnClick = UpdateImage
        end
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 209
    Top = 24
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 241
    Top = 24
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 273
    Top = 24
  end
end
