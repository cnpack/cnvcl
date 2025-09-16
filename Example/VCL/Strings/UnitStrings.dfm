object FormStrings: TFormStrings
  Left = 192
  Top = 107
  Width = 860
  Height = 561
  Caption = 'Strings Test for Unicode & Non-Unicode Compiler'
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
  object pgcStrings: TPageControl
    Left = 8
    Top = 8
    Width = 833
    Height = 513
    ActivePage = tsStringBuilder
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsStringBuilder: TTabSheet
      Caption = 'String Builder'
      object grpStringBuilder: TGroupBox
        Left = 8
        Top = 8
        Width = 809
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'String Builder'
        TabOrder = 0
        object lblStringBuilderInfo: TLabel
          Left = 608
          Top = 32
          Width = 3
          Height = 13
        end
        object rgMode: TRadioGroup
          Left = 16
          Top = 24
          Width = 81
          Height = 97
          Caption = 'Mode'
          ItemIndex = 0
          Items.Strings = (
            'Auto'
            'Ansi'
            'Wide')
          TabOrder = 0
          OnClick = rgModeClick
        end
        object btnBoolean: TButton
          Left = 520
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Boolean'
          TabOrder = 6
          OnClick = btnBooleanClick
        end
        object btnByte: TButton
          Left = 440
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Byte'
          TabOrder = 5
          OnClick = btnByteClick
        end
        object btnChar: TButton
          Left = 360
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Char'
          TabOrder = 4
          OnClick = btnCharClick
        end
        object btnCurrency: TButton
          Left = 280
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Currency'
          TabOrder = 3
          OnClick = btnCurrencyClick
        end
        object btnDouble: TButton
          Left = 200
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Double'
          TabOrder = 2
          OnClick = btnDoubleClick
        end
        object btnSmallint: TButton
          Left = 520
          Top = 64
          Width = 75
          Height = 25
          Caption = 'SmallInt'
          TabOrder = 12
          OnClick = btnSmallintClick
        end
        object btnInteger: TButton
          Left = 440
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Integer'
          TabOrder = 11
          OnClick = btnIntegerClick
        end
        object btnInt64: TButton
          Left = 360
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Int64'
          TabOrder = 10
          OnClick = btnInt64Click
        end
        object btnTObject: TButton
          Left = 280
          Top = 64
          Width = 75
          Height = 25
          Caption = 'TObject'
          TabOrder = 9
        end
        object btnShortint: TButton
          Left = 200
          Top = 64
          Width = 75
          Height = 25
          Caption = 'ShortInt'
          TabOrder = 8
          OnClick = btnShortintClick
        end
        object btnSingle: TButton
          Left = 120
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Single'
          TabOrder = 7
          OnClick = btnSingleClick
        end
        object btnUInt64: TButton
          Left = 520
          Top = 96
          Width = 75
          Height = 25
          Caption = 'UInt64'
          TabOrder = 18
          OnClick = btnUInt64Click
        end
        object btnWord: TButton
          Left = 440
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Word'
          TabOrder = 17
          OnClick = btnWordClick
        end
        object btnCardinal: TButton
          Left = 360
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Cardinal'
          TabOrder = 16
          OnClick = btnCardinalClick
        end
        object btnPAnsiChar: TButton
          Left = 280
          Top = 96
          Width = 75
          Height = 25
          Caption = 'PAnsiChar'
          TabOrder = 15
          OnClick = btnPAnsiCharClick
        end
        object btnCharRepeat: TButton
          Left = 200
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Char Repeat'
          TabOrder = 14
          OnClick = btnCharRepeatClick
        end
        object btnStringStartCount: TButton
          Left = 120
          Top = 96
          Width = 75
          Height = 25
          Caption = 'string Partly'
          TabOrder = 13
          OnClick = btnStringStartCountClick
        end
        object btnFormat: TButton
          Left = 120
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Format'
          TabOrder = 1
          OnClick = btnFormatClick
        end
        object mmoContent: TMemo
          Left = 16
          Top = 136
          Width = 577
          Height = 97
          TabOrder = 19
        end
      end
    end
    object tsStrings: TTabSheet
      Caption = 'Strings'
      ImageIndex = 1
      object btnTestPosEx: TButton
        Left = 16
        Top = 16
        Width = 89
        Height = 25
        Caption = 'Test CnPosEx'
        TabOrder = 0
        OnClick = btnTestPosExClick
      end
      object mmoStringsRes: TMemo
        Left = 16
        Top = 56
        Width = 497
        Height = 409
        TabOrder = 1
      end
    end
  end
end
