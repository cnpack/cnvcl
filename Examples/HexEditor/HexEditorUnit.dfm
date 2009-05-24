object FormHexEditor: TFormHexEditor
  Left = 439
  Top = 188
  Width = 439
  Height = 275
  Caption = 'FormHexEditor'
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
  object StatusBarOne: TStatusBar
    Left = 0
    Top = 229
    Width = 431
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object FHexEditor: TCnHexEditor
    Left = 0
    Top = 0
    Width = 431
    Height = 229
    Align = alClient
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ParentColor = False
    TabOrder = 1
  end
end
