object CnADOUpdateSQLForm: TCnADOUpdateSQLForm
  Left = 367
  Top = 254
  BorderStyle = bsDialog
  Caption = 'SQL Editor for CnADOUpdateSQL '
  ClientHeight = 320
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 473
    Height = 277
    ActivePage = TabSheetOptions
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheetOptions: TTabSheet
      Caption = 'Optio&ns'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 449
        Height = 233
        Caption = 'SQL &Generation'
        TabOrder = 0
        object lbl1: TLabel
          Left = 8
          Top = 16
          Width = 60
          Height = 13
          Caption = 'Table &Name:'
          FocusControl = cbbTables
        end
        object lbl2: TLabel
          Left = 160
          Top = 16
          Width = 52
          Height = 13
          Caption = '&Key Fields:'
          FocusControl = lstKeyFields
        end
        object lbl3: TLabel
          Left = 304
          Top = 16
          Width = 69
          Height = 13
          Caption = 'Update &Fields:'
          FocusControl = lstUpdateFields
        end
        object cbbTables: TComboBox
          Left = 8
          Top = 32
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbbTablesChange
        end
        object btnGetTables: TButton
          Left = 16
          Top = 64
          Width = 121
          Height = 21
          Caption = 'Get Tables'
          TabOrder = 1
          OnClick = btnGetTablesClick
        end
        object btnGetTableFields: TButton
          Left = 16
          Top = 96
          Width = 121
          Height = 21
          Caption = 'Get Table Fields'
          TabOrder = 2
          OnClick = btnGetTableFieldsClick
        end
        object btnGenerateSQL: TButton
          Left = 16
          Top = 128
          Width = 121
          Height = 21
          Caption = 'Generate SQL'
          TabOrder = 3
          OnClick = btnGenerateSQLClick
        end
        object lstKeyFields: TListBox
          Left = 160
          Top = 32
          Width = 137
          Height = 193
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = pmKeyFields
          TabOrder = 4
        end
        object lstUpdateFields: TListBox
          Left = 304
          Top = 32
          Width = 137
          Height = 193
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = pmUpdateFields
          TabOrder = 5
        end
      end
    end
    object TabSheetSQL: TTabSheet
      Caption = '&SQL'
      ImageIndex = 1
      object lbl4: TLabel
        Left = 8
        Top = 72
        Width = 48
        Height = 13
        Caption = 'S&QL Text:'
      end
      object RadioGroupSQL: TRadioGroup
        Left = 8
        Top = 8
        Width = 449
        Height = 49
        Caption = 'Statement Type'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          '&Modify'
          '&Insert'
          '&Delete')
        TabOrder = 0
        OnClick = RadioGroupSQLClick
      end
      object mmoSQLText: TMemo
        Left = 8
        Top = 88
        Width = 449
        Height = 145
        TabOrder = 1
        OnChange = mmoSQLTextChange
      end
    end
  end
  object btnHelp: TButton
    Left = 405
    Top = 292
    Width = 75
    Height = 21
    Caption = '&Help'
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 318
    Top = 292
    Width = 75
    Height = 21
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 230
    Top = 292
    Width = 75
    Height = 21
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object pmKeyFields: TPopupMenu
    Left = 188
    Top = 223
    object mniSelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = mniSelectAll1Click
    end
    object mniClearAll1: TMenuItem
      Caption = 'Clear All'
      OnClick = mniClearAll1Click
    end
  end
  object pmUpdateFields: TPopupMenu
    Left = 332
    Top = 223
    object mniSelectAll2: TMenuItem
      Caption = 'Select All'
      OnClick = mniSelectAll2Click
    end
    object mniClearAll2: TMenuItem
      Caption = 'Clear All'
      OnClick = mniClearAll2Click
    end
  end
end
