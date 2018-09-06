object FormEcc: TFormEcc
  Left = 216
  Top = 122
  Width = 703
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
    Width = 657
    Height = 465
    ActivePage = tsSimpleECC
    TabOrder = 0
    object tsSimpleECC: TTabSheet
      Caption = 'Simple ECC'
      object lblPrivateKey: TLabel
        Left = 448
        Top = 196
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblPublicKey: TLabel
        Left = 528
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
      object grpSimpleECC: TGroupBox
        Left = 16
        Top = 16
        Width = 617
        Height = 161
        Caption = 'Simple E23(1,1)'
        TabOrder = 0
        object Bevel1: TBevel
          Left = 20
          Top = 64
          Width = 553
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
          Left = 116
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 0+P'
          TabOrder = 1
          OnClick = btnTest0Click
        end
        object btnTestOn: TButton
          Left = 212
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test On Curve'
          TabOrder = 2
          OnClick = btnTestOnClick
        end
        object btnTestInverse: TButton
          Left = 308
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Inverse'
          TabOrder = 3
          OnClick = btnTestInverseClick
        end
        object btnTest2P: TButton
          Left = 402
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 2*P'
          TabOrder = 4
          OnClick = btnTest2PClick
        end
        object btnTestMul: TButton
          Left = 500
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
        Left = 488
        Top = 192
        Width = 33
        Height = 21
        TabOrder = 3
      end
      object edtPublicKey: TEdit
        Left = 568
        Top = 192
        Width = 65
        Height = 21
        TabOrder = 4
      end
      object edtData: TEdit
        Left = 400
        Top = 232
        Width = 33
        Height = 21
        TabOrder = 5
        Text = '7'
      end
      object btnEncrypt: TButton
        Left = 448
        Top = 232
        Width = 75
        Height = 21
        Caption = 'Encrypt'
        TabOrder = 6
        OnClick = btnEncryptClick
      end
      object edtEncrypted: TEdit
        Left = 536
        Top = 232
        Width = 97
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
        Width = 97
        Height = 21
        TabOrder = 9
      end
    end
  end
end
