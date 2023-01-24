object frmMain: TfrmMain
  Left = 31
  Top = 38
  Width = 973
  Height = 617
  Caption = 'OpenGLPaintBox Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 965
    Height = 590
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet2: TTabSheet
      Caption = '速度与列表测试'
      ImageIndex = 1
      object imgBench: TImage
        Left = 389
        Top = 24
        Width = 380
        Height = 380
      end
      object lblGLPaintBoxBench: TLabel
        Left = 3
        Top = 5
        Width = 54
        Height = 13
        Caption = 'OpenGLPaintBox'
      end
      object lblGDIBench: TLabel
        Left = 389
        Top = 5
        Width = 18
        Height = 13
        Caption = 'GDI'
      end
      object Label8: TLabel
        Left = 427
        Top = 472
        Width = 430
        Height = 39
        Caption = 
          '由于本样例程序里用到了多个OpenGLPaintBox共同工作，所以在CnOpenGLPain' +
          'tBox单元中定义了MultiCanvas指示符。在大多数情况下程序不会有多个' +
          '画布，所以如果你的程序不需要使用多个画布，可以取消定义该指示符，' +
          '能够轻微提升效率。'
        WordWrap = True
      end
      object glcBench: TCnOpenGLPaintBox
        Left = 3
        Top = 24
        Width = 380
        Height = 380
        BackgroundColor = clWhite
        Antialiasing = False
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 410
        Width = 414
        Height = 95
        Caption = '速度测试'
        TabOrder = 1
        object Label6: TLabel
          Left = 13
          Top = 53
          Width = 242
          Height = 13
          Caption = '请点两次“20000直线”，第二次的时间更为准确'
        end
        object btnBenchLines: TButton
          Left = 5
          Top = 22
          Width = 80
          Height = 25
          Caption = '20000直线'
          TabOrder = 0
          OnClick = BenchTestClick
        end
        object btnBenchRects: TButton
          Tag = 1
          Left = 86
          Top = 22
          Width = 80
          Height = 25
          Caption = '20000矩形'
          TabOrder = 1
          OnClick = BenchTestClick
        end
        object btnBenchEllipses: TButton
          Tag = 2
          Left = 167
          Top = 22
          Width = 80
          Height = 25
          Caption = '20000椭圆'
          TabOrder = 2
          OnClick = BenchTestClick
        end
        object btnBenchPoints: TButton
          Tag = 3
          Left = 248
          Top = 22
          Width = 80
          Height = 25
          Caption = '200000点'
          TabOrder = 3
          OnClick = BenchTestClick
        end
        object btnBenchTexts: TButton
          Tag = 4
          Left = 329
          Top = 22
          Width = 80
          Height = 25
          Caption = '10000文字'
          TabOrder = 4
          OnClick = BenchTestClick
        end
        object rdoPenWidth1: TRadioButton
          Left = 13
          Top = 72
          Width = 89
          Height = 17
          Caption = 'Pen Width = 1'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object rdoPenWidth2: TRadioButton
          Left = 117
          Top = 72
          Width = 89
          Height = 17
          Caption = 'Pen Width = 2'
          TabOrder = 6
        end
      end
      object GroupBox2: TGroupBox
        Left = 423
        Top = 410
        Width = 426
        Height = 53
        Caption = '绘制列表测试'
        TabOrder = 2
        object Label7: TLabel
          Left = 269
          Top = 27
          Width = 48
          Height = 13
          Caption = '画笔宽度'
        end
        object lbl1: TLabel
          Left = 180
          Top = 27
          Width = 48
          Height = 13
          Caption = '画笔颜色'
        end
        object btnBuildList: TButton
          Tag = 4
          Left = 5
          Top = 22
          Width = 80
          Height = 25
          Caption = '创建列表'
          TabOrder = 0
          OnClick = btnBuildListClick
        end
        object btnExecuteList: TButton
          Tag = 4
          Left = 91
          Top = 22
          Width = 80
          Height = 25
          Caption = '执行列表'
          Enabled = False
          TabOrder = 1
          OnClick = btnExecuteListClick
        end
        object trackPenWidth2: TTrackBar
          Left = 323
          Top = 24
          Width = 99
          Height = 20
          Min = 1
          Orientation = trHorizontal
          Frequency = 1
          Position = 1
          SelEnd = 0
          SelStart = 0
          TabOrder = 2
          ThumbLength = 14
          TickMarks = tmBottomRight
          TickStyle = tsAuto
        end
        object pnlPenColor2: TPanel
          Left = 232
          Top = 20
          Width = 25
          Height = 25
          BevelOuter = bvLowered
          Color = clGrayText
          TabOrder = 3
          OnClick = pnlPenColor2Click
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = '综合测试'
      object glcMain: TCnOpenGLPaintBox
        Left = 0
        Top = 0
        Width = 957
        Height = 507
        BackgroundColor = clWhite
        Align = alClient
        OnResize = glcMainResize
      end
      object Panel1: TPanel
        Left = 0
        Top = 507
        Width = 957
        Height = 55
        Align = alBottom
        TabOrder = 1
        object Label1: TLabel
          Left = 348
          Top = 7
          Width = 48
          Height = 13
          Caption = '画笔颜色'
        end
        object Label2: TLabel
          Left = 348
          Top = 33
          Width = 48
          Height = 13
          Caption = '填充颜色'
        end
        object Label3: TLabel
          Left = 504
          Top = 7
          Width = 48
          Height = 13
          Caption = '画笔类型'
        end
        object Label4: TLabel
          Left = 664
          Top = 7
          Width = 48
          Height = 13
          Caption = '画笔宽度'
        end
        object Label5: TLabel
          Left = 503
          Top = 33
          Width = 36
          Height = 13
          Caption = '透明度'
        end
        object chkAntialiasing: TCheckBox
          Left = 826
          Top = 6
          Width = 89
          Height = 17
          Caption = '抗锯齿'
          Checked = True
          State = cbChecked
          TabOrder = 8
          OnClick = MainTestClick
        end
        object btnDot: TButton
          Left = 1
          Top = 2
          Width = 66
          Height = 25
          Caption = '点'
          TabOrder = 0
          OnClick = MainTestClick
        end
        object cboLineStipple: TComboBox
          Left = 560
          Top = 4
          Width = 89
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 9
          OnChange = MainTestClick
          Items.Strings = (
            '实线'
            'Dash'
            'DashDot'
            'lssDashDotDot'
            'lssDot')
        end
        object btnLine: TButton
          Tag = 1
          Left = 1
          Top = 28
          Width = 66
          Height = 25
          Caption = '直线'
          TabOrder = 1
          OnClick = MainTestClick
        end
        object btnPolyline: TButton
          Tag = 2
          Left = 68
          Top = 2
          Width = 104
          Height = 25
          Caption = '折线、多边形'
          TabOrder = 2
          OnClick = MainTestClick
        end
        object btnCurve: TButton
          Tag = 3
          Left = 68
          Top = 28
          Width = 104
          Height = 25
          Caption = '曲线、闭合曲线'
          TabOrder = 3
          OnClick = MainTestClick
        end
        object btnRectEllipse: TButton
          Tag = 4
          Left = 173
          Top = 2
          Width = 89
          Height = 25
          Caption = '矩形、椭圆'
          TabOrder = 4
          OnClick = MainTestClick
        end
        object btnArc: TButton
          Tag = 5
          Left = 173
          Top = 28
          Width = 89
          Height = 25
          Caption = '弧'
          TabOrder = 5
          OnClick = MainTestClick
        end
        object btnText: TButton
          Tag = 6
          Left = 263
          Top = 2
          Width = 75
          Height = 25
          Caption = '文字'
          TabOrder = 6
          OnClick = MainTestClick
        end
        object btnImage: TButton
          Tag = 7
          Left = 263
          Top = 28
          Width = 75
          Height = 25
          Caption = '图片'
          TabOrder = 7
          OnClick = MainTestClick
        end
        object chkTranslate: TCheckBox
          Left = 664
          Top = 32
          Width = 89
          Height = 17
          Caption = '位移变换'
          TabOrder = 11
          OnClick = MainTestClick
        end
        object chkRotate: TCheckBox
          Left = 745
          Top = 32
          Width = 89
          Height = 17
          Caption = '旋转变换'
          TabOrder = 12
          OnClick = MainTestClick
        end
        object chkScale: TCheckBox
          Left = 826
          Top = 32
          Width = 89
          Height = 17
          Caption = '放缩变换'
          TabOrder = 14
          OnClick = MainTestClick
        end
        object trackPenWidth: TTrackBar
          Left = 718
          Top = 4
          Width = 99
          Height = 20
          Min = 1
          Orientation = trHorizontal
          Frequency = 1
          Position = 1
          SelEnd = 0
          SelStart = 0
          TabOrder = 10
          ThumbLength = 14
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = MainTestClick
        end
        object trackFillAlpha: TTrackBar
          Left = 557
          Top = 30
          Width = 99
          Height = 20
          Max = 255
          Orientation = trHorizontal
          Frequency = 10
          Position = 255
          SelEnd = 0
          SelStart = 0
          TabOrder = 13
          ThumbLength = 14
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = MainTestClick
        end
        object pnlPen: TPanel
          Left = 408
          Top = 8
          Width = 81
          Height = 17
          BevelOuter = bvLowered
          Color = clRed
          TabOrder = 15
          OnClick = pnlPenColor2Click
        end
        object pnlBrush: TPanel
          Left = 408
          Top = 32
          Width = 81
          Height = 17
          BevelOuter = bvLowered
          Color = clGreen
          TabOrder = 16
          OnClick = pnlPenColor2Click
        end
      end
    end
  end
  object dlgColor1: TColorDialog
    Ctl3D = True
    Left = 396
    Top = 536
  end
end
