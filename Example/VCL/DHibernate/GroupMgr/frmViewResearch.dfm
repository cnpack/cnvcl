object FormViewResearch: TFormViewResearch
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '查看成果'
  ClientHeight = 292
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbView: TGroupBox
    Left = 0
    Top = 167
    Width = 390
    Height = 125
    Align = alBottom
    Caption = '警告原因明细'
    TabOrder = 0
    object pnlBtn: TPanel
      Left = 296
      Top = 15
      Width = 92
      Height = 108
      Align = alRight
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object btnClose: TButton
        Left = 6
        Top = 72
        Width = 75
        Height = 25
        Caption = '关闭'
        TabOrder = 0
        OnClick = btnCloseClick
      end
      object btnDelete: TButton
        Left = 6
        Top = 41
        Width = 75
        Height = 25
        Caption = '删除'
        TabOrder = 1
        OnClick = btnDeleteClick
      end
    end
    object mmContext: TDBMemo
      Left = 2
      Top = 15
      Width = 294
      Height = 108
      Align = alClient
      DataField = 'Context'
      DataSource = dsResearch
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WantTabs = True
    end
  end
  object gbPager: TGroupBox
    Left = 0
    Top = 125
    Width = 390
    Height = 42
    Align = alBottom
    TabOrder = 1
    object btnFirst: TButton
      Left = 59
      Top = 6
      Width = 75
      Height = 25
      Caption = '第一页'
      TabOrder = 0
      OnClick = btnFirstClick
    end
    object btnPrior: TButton
      Left = 140
      Top = 6
      Width = 75
      Height = 25
      Caption = '上一页'
      TabOrder = 1
      OnClick = btnPriorClick
    end
    object btnNext: TButton
      Left = 221
      Top = 6
      Width = 75
      Height = 25
      Caption = '下一页'
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnLast: TButton
      Left = 302
      Top = 6
      Width = 75
      Height = 25
      Caption = '最后一页'
      TabOrder = 3
      OnClick = btnLastClick
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 390
    Height = 125
    Align = alClient
    DataSource = dsResearch
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'QQCode'
        Title.Alignment = taCenter
        Title.Caption = 'QQ 号码'
        Width = 90
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'UserName_FORMULA'
        Title.Alignment = taCenter
        Title.Caption = '用户名'
        Width = 90
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NameCard_FORMULA'
        Title.Alignment = taCenter
        Title.Caption = '群名片'
        Width = 90
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Time'
        Title.Alignment = taCenter
        Title.Caption = '成果发布时间'
        Width = 120
        Visible = True
      end>
  end
  object qryResearch: TCnDHibernateQueryAdvance
    Active = False
    Aggregates = <>
    Params = <>
    Connection = FormMain.Connection
    SQL.Strings = (
      'select * from Researchs')
    CurrentPage = 0
    RowsPerPage = 5
    TableName = 'Researchs'
    useFormula = True
    PKName = 'SelfId'
    Left = 72
    Top = 64
  end
  object dsResearch: TDataSource
    DataSet = qryResearch
    Left = 104
    Top = 64
  end
end
