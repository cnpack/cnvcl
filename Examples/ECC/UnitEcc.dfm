object FormEcc: TFormEcc
  Left = 134
  Top = 131
  Width = 777
  Height = 528
  Caption = 'ECC Test'
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
  object pgc1: TPageControl
    Left = 16
    Top = 16
    Width = 737
    Height = 465
    ActivePage = tsSimpleECC
    TabOrder = 0
    object tsSimpleECC: TTabSheet
      Caption = 'Simple ECC'
      object lblPrivateKey: TLabel
        Left = 456
        Top = 196
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblPublicKey: TLabel
        Left = 584
        Top = 196
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblData: TLabel
        Left = 360
        Top = 236
        Width = 26
        Height = 13
        Caption = 'Data:'
      end
      object Bevel2: TBevel
        Left = 400
        Top = 320
        Width = 313
        Height = 17
        Shape = bsTopLine
      end
      object lblECDH: TLabel
        Left = 360
        Top = 312
        Width = 33
        Height = 13
        Caption = 'ECDH:'
      end
      object lblDHA: TLabel
        Left = 360
        Top = 352
        Width = 10
        Height = 13
        Caption = 'A:'
      end
      object lblDHB: TLabel
        Left = 360
        Top = 392
        Width = 10
        Height = 13
        Caption = 'B:'
      end
      object lblXb: TLabel
        Left = 376
        Top = 392
        Width = 13
        Height = 13
        Caption = 'Xb'
      end
      object lblXA: TLabel
        Left = 376
        Top = 352
        Width = 13
        Height = 13
        Caption = 'Xa'
      end
      object grpSimpleECC: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 161
        Caption = 'Simple E23(1,1) Ò²¾ÍÊÇ Y^2 = X^3 + X + 1 mod 23'
        TabOrder = 0
        object Bevel1: TBevel
          Left = 20
          Top = 64
          Width = 653
          Height = 17
          Shape = bsTopLine
        end
        object lblPX: TLabel
          Left = 24
          Top = 92
          Width = 20
          Height = 13
          Caption = 'P.X:'
        end
        object lblPY: TLabel
          Left = 112
          Top = 92
          Width = 20
          Height = 13
          Caption = 'P.Y:'
        end
        object lblAdd: TLabel
          Left = 216
          Top = 92
          Width = 42
          Height = 13
          Caption = '+     Q.X:'
        end
        object lblQY: TLabel
          Left = 328
          Top = 92
          Width = 21
          Height = 13
          Caption = 'Q.Y:'
        end
        object lblAddResult: TLabel
          Left = 518
          Top = 92
          Width = 3
          Height = 13
        end
        object lblMPX: TLabel
          Left = 24
          Top = 128
          Width = 20
          Height = 13
          Caption = 'P.X:'
        end
        object lblMPY: TLabel
          Left = 112
          Top = 128
          Width = 20
          Height = 13
          Caption = 'P.Y:'
        end
        object lblMul: TLabel
          Left = 216
          Top = 128
          Width = 29
          Height = 13
          Caption = '*      K'
        end
        object lblMResult: TLabel
          Left = 518
          Top = 128
          Width = 3
          Height = 13
        end
        object btnPOn: TSpeedButton
          Left = 194
          Top = 88
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnPOnClick
        end
        object btnMPOn: TSpeedButton
          Left = 194
          Top = 124
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnMPOnClick
        end
        object btnQOn: TSpeedButton
          Left = 410
          Top = 88
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnQOnClick
        end
        object btnTest1: TButton
          Left = 20
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test P+Q'
          TabOrder = 0
          OnClick = btnTest1Click
        end
        object btnTest0: TButton
          Left = 135
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 0+P'
          TabOrder = 1
          OnClick = btnTest0Click
        end
        object btnTestOn: TButton
          Left = 250
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test On Curve'
          TabOrder = 2
          OnClick = btnTestOnClick
        end
        object btnTestInverse: TButton
          Left = 366
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Inverse'
          TabOrder = 3
          OnClick = btnTestInverseClick
        end
        object btnTest2P: TButton
          Left = 481
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 2*P'
          TabOrder = 4
          OnClick = btnTest2PClick
        end
        object btnTestMul: TButton
          Left = 596
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test n*P'
          TabOrder = 5
          OnClick = btnTestMulClick
        end
        object edtPX: TEdit
          Left = 56
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 6
          Text = '6'
        end
        object edtPY: TEdit
          Left = 144
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 7
          Text = '19'
        end
        object edtQX: TEdit
          Left = 272
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 8
          Text = '9'
        end
        object edtQY: TEdit
          Left = 360
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 9
          Text = '7'
        end
        object btnEqual: TButton
          Left = 432
          Top = 88
          Width = 65
          Height = 21
          Caption = '='
          TabOrder = 10
          OnClick = btnEqualClick
        end
        object edtMPX: TEdit
          Left = 56
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 11
          Text = '9'
        end
        object edtMPY: TEdit
          Left = 144
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 12
          Text = '7'
        end
        object btnMEqual: TButton
          Left = 432
          Top = 124
          Width = 65
          Height = 21
          Caption = '='
          TabOrder = 13
          OnClick = btnMEqualClick
        end
        object edtMK: TEdit
          Left = 272
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 14
          Text = '3'
        end
      end
      object chtE2311: TChart
        Left = 0
        Top = 177
        Width = 345
        Height = 266
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Title.AdjustFrame = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.Automatic = False
        BottomAxis.AutomaticMaximum = False
        BottomAxis.AutomaticMinimum = False
        BottomAxis.Maximum = 23
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.ExactDateTime = False
        LeftAxis.Increment = 1
        LeftAxis.LabelsSeparation = 0
        LeftAxis.Maximum = 23
        View3D = False
        BevelOuter = bvNone
        TabOrder = 1
        object pntsrsSeries1: TPointSeries
          Marks.ArrowLength = 0
          Marks.Visible = False
          SeriesColor = clRed
          ShowInLegend = False
          Pointer.Dark3D = False
          Pointer.Draw3D = False
          Pointer.HorizSize = 3
          Pointer.InflateMargins = False
          Pointer.Pen.Style = psDashDot
          Pointer.Pen.Visible = False
          Pointer.Style = psRectangle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.DateTime = False
          XValues.Name = 'X'
          XValues.Multiplier = 1
          XValues.Order = loAscending
          YValues.DateTime = False
          YValues.Name = 'Y'
          YValues.Multiplier = 1
          YValues.Order = loNone
        end
      end
      object btnNewKey: TButton
        Left = 360
        Top = 192
        Width = 75
        Height = 21
        Caption = 'New Key'
        TabOrder = 2
        OnClick = btnNewKeyClick
      end
      object edtPrivateKey: TEdit
        Left = 504
        Top = 192
        Width = 65
        Height = 21
        TabOrder = 3
      end
      object edtPublicKey: TEdit
        Left = 624
        Top = 192
        Width = 89
        Height = 21
        TabOrder = 4
      end
      object edtData: TEdit
        Left = 400
        Top = 232
        Width = 73
        Height = 21
        TabOrder = 5
        Text = '7'
      end
      object btnEncrypt: TButton
        Left = 504
        Top = 232
        Width = 75
        Height = 21
        Caption = 'Encrypt'
        TabOrder = 6
        OnClick = btnEncryptClick
      end
      object edtEncrypted: TEdit
        Left = 600
        Top = 232
        Width = 113
        Height = 21
        TabOrder = 7
      end
      object btnDecrypt: TButton
        Left = 360
        Top = 272
        Width = 75
        Height = 21
        Caption = 'Decrypt'
        TabOrder = 8
        OnClick = btnDecryptClick
      end
      object edtDecrypted: TEdit
        Left = 448
        Top = 272
        Width = 177
        Height = 21
        TabOrder = 9
      end
      object btnBatchVerify: TButton
        Left = 640
        Top = 272
        Width = 75
        Height = 21
        Caption = 'Batch Verify'
        TabOrder = 10
        OnClick = btnBatchVerifyClick
      end
      object edtDHXa: TEdit
        Left = 400
        Top = 348
        Width = 41
        Height = 21
        TabOrder = 11
        Text = '3'
      end
      object edtDHXb: TEdit
        Left = 400
        Top = 388
        Width = 41
        Height = 21
        TabOrder = 12
        Text = '5'
      end
      object btnCalcYb: TButton
        Left = 450
        Top = 388
        Width = 49
        Height = 21
        Caption = 'Calc Yb'
        TabOrder = 13
        OnClick = btnCalcYbClick
      end
      object btnCalcXA: TButton
        Left = 450
        Top = 348
        Width = 49
        Height = 21
        Caption = 'Calc Ya'
        TabOrder = 14
        OnClick = btnCalcXAClick
      end
      object edtDHYa: TEdit
        Left = 506
        Top = 348
        Width = 63
        Height = 21
        TabOrder = 15
      end
      object edtDHYb: TEdit
        Left = 506
        Top = 388
        Width = 63
        Height = 21
        TabOrder = 16
      end
      object btnDHBCK: TButton
        Left = 582
        Top = 388
        Width = 65
        Height = 21
        Caption = 'B Calc Key'
        TabOrder = 17
        OnClick = btnDHBCKClick
      end
      object btnDHACKey: TButton
        Left = 582
        Top = 348
        Width = 65
        Height = 21
        Caption = 'A Calc Key'
        TabOrder = 18
        OnClick = btnDHACKeyClick
      end
      object edtAKey: TEdit
        Left = 654
        Top = 348
        Width = 59
        Height = 21
        TabOrder = 19
      end
      object edtBKey: TEdit
        Left = 654
        Top = 388
        Width = 59
        Height = 21
        TabOrder = 20
      end
    end
    object tsInt64ECC: TTabSheet
      Caption = 'Intt64 ECC'
      ImageIndex = 1
      object grpGenEcc: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 401
        Caption = 'Generated Ecc'
        TabOrder = 0
        object lblEccY2: TLabel
          Left = 16
          Top = 28
          Width = 221
          Height = 13
          Caption = 'Y ^ 2 = X ^ 3    +                X +                 mod  '
        end
        object lblEccG: TLabel
          Left = 312
          Top = 28
          Width = 11
          Height = 13
          Caption = 'G:'
        end
        object lblEccOrder: TLabel
          Left = 416
          Top = 28
          Width = 51
          Height = 13
          Caption = 'with Order:'
        end
        object Bevel3: TBevel
          Left = 16
          Top = 64
          Width = 537
          Height = 17
          Shape = bsTopLine
        end
        object btnGenEcc: TButton
          Left = 568
          Top = 24
          Width = 107
          Height = 21
          Caption = 'Generate Small Ecc'
          TabOrder = 0
          OnClick = btnGenEccClick
        end
        object edtEccA: TEdit
          Left = 104
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 1
          Text = '4'
        end
        object edtEccB: TEdit
          Left = 168
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 2
          Text = '182'
        end
        object edtEccP: TEdit
          Left = 240
          Top = 24
          Width = 41
          Height = 21
          TabOrder = 3
          Text = '661'
        end
        object edtEccGX: TEdit
          Left = 328
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 4
          Text = '1'
        end
        object edtEccGY: TEdit
          Left = 368
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 5
          Text = '214'
        end
        object edtEccOrder: TEdit
          Left = 480
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 6
          Text = '673'
        end
        object btnCalcNG: TButton
          Left = 568
          Top = 56
          Width = 105
          Height = 21
          Caption = 'Calculate N*G'
          TabOrder = 7
          OnClick = btnCalcNGClick
        end
        object mmoGenECCPoints: TMemo
          Left = 568
          Top = 96
          Width = 105
          Height = 289
          ScrollBars = ssVertical
          TabOrder = 8
        end
        object chtEccInt64: TChart
          Left = 8
          Top = 72
          Width = 393
          Height = 321
          BackWall.Brush.Color = clWhite
          BackWall.Brush.Style = bsClear
          MarginBottom = 0
          MarginLeft = 0
          MarginRight = 2
          Title.AdjustFrame = False
          Title.Text.Strings = (
            'TChart')
          Title.Visible = False
          BottomAxis.Automatic = False
          BottomAxis.AutomaticMaximum = False
          BottomAxis.AutomaticMinimum = False
          BottomAxis.Maximum = 1000
          LeftAxis.Automatic = False
          LeftAxis.AutomaticMaximum = False
          LeftAxis.AutomaticMinimum = False
          LeftAxis.Maximum = 1000
          Legend.Visible = False
          View3D = False
          View3DWalls = False
          BevelOuter = bvNone
          TabOrder = 9
          object pntsrsSeries2: TPointSeries
            Marks.ArrowLength = 0
            Marks.Visible = False
            SeriesColor = clRed
            Pointer.Brush.Color = clRed
            Pointer.HorizSize = 2
            Pointer.InflateMargins = False
            Pointer.Pen.Visible = False
            Pointer.Style = psCircle
            Pointer.VertSize = 2
            Pointer.Visible = True
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Y'
            YValues.Multiplier = 1
            YValues.Order = loNone
          end
        end
      end
    end
  end
end
