object FormJSON: TFormJSON
  Left = 192
  Top = 138
  Width = 1051
  Height = 638
  Caption = 'JSON Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 985
    Height = 569
    ActivePage = tsJSONParse
    TabOrder = 0
    object tsJSONParse: TTabSheet
      Caption = 'JSON Parse'
      object lblReconstruct: TLabel
        Left = 464
        Top = 280
        Width = 61
        Height = 13
        Caption = 'Reconstruct:'
      end
      object lblToken: TLabel
        Left = 8
        Top = 280
        Width = 34
        Height = 13
        Caption = 'Token:'
      end
      object lblStr: TLabel
        Left = 16
        Top = 16
        Width = 30
        Height = 13
        Caption = 'String:'
      end
      object mmoJSONReconstruct: TMemo
        Left = 544
        Top = 280
        Width = 417
        Height = 241
        ReadOnly = True
        TabOrder = 0
      end
      object chkJSONFormat: TCheckBox
        Left = 456
        Top = 256
        Width = 73
        Height = 17
        Caption = 'Format'
        TabOrder = 1
      end
      object tvJSON: TTreeView
        Left = 544
        Top = 16
        Width = 417
        Height = 241
        Indent = 19
        TabOrder = 2
      end
      object mmoJSON: TMemo
        Left = 56
        Top = 16
        Width = 385
        Height = 241
        Lines.Strings = (
          '{'
          '    "animals": {'
          '        "dog": ['
          '            {'
          '                "name": "Ru³Ô·¹fus",'
          '                "age":15'
          '            },'
          '            {'
          '                "name": "Ma\u45A3rty",'
          '                "age": null,'
          '    "die": true'
          '            }'
          '        ]'
          '    }'
          '}')
        TabOrder = 3
      end
      object btnParse: TButton
        Left = 456
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Parse'
        TabOrder = 4
        OnClick = btnParseClick
      end
      object mmoJSONToken: TMemo
        Left = 56
        Top = 280
        Width = 385
        Height = 241
        ScrollBars = ssVertical
        TabOrder = 5
      end
    end
  end
end
