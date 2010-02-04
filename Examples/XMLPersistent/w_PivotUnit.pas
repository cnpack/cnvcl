unit w_PivotUnit;

interface

uses Classes, SysUtils;

type

  TPivotCache = class(TCollectionItem)
  end;

  TPivotItem = class(TCollectionItem)
  private
    FItemName: widestring;
    FVisible: Boolean;
  published
    property ItemName: widestring read FItemName write FItemName;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TPivotField = class(TCollectionItem)
  private
    FAutoShowCount: LongWord;
    FAutoShowField: WideString;
    FBlankLine: Boolean;
    FCalculation: LongWord;
    FCaption: widestring;
    FCurrentPage: Variant;
    FFieldName: WideString;
    FFunctionType: LongWord;
    FOrientation: LongWord;
    FPivotItems: TCollection;
    FPosition: LongWord;
    FRange: LongWord;
    FAutoSortOrder: LongWord;
    FSortField: widestring;
    FSourceName: widestring;
    function GetPivotItem(Index: Integer): TPivotItem;
    procedure SetAutoShowField(const Value: WideString);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property PivotItem[Index: Integer]: TPivotItem read GetPivotItem; default;
  published
    property AutoShowCount: LongWord read FAutoShowCount write FAutoShowCount;
    property AutoShowField: WideString read FAutoShowField write SetAutoShowField;
    property BlankLine: Boolean read FBlankLine write FBlankLine;
    property Calculation: LongWord read FCalculation write FCalculation;
    property Caption: widestring read FCaption write FCaption;
    property CurrentPage: Variant read FCurrentPage write FCurrentPage;
    property FieldName: WideString read FFieldName write FFieldName;
    property FunctionType: LongWord read FFunctionType write FFunctionType;
    property Orientation: LongWord read FOrientation write FOrientation;
    property PivotItems: TCollection read FPivotItems write FPivotItems;
    property Position: LongWord read FPosition write FPosition;
    property Range: LongWord read FRange write FRange;
    property AutoSortOrder: LongWord read FAutoSortOrder write FAutoSortOrder;
    property SortField: widestring read FSortField write FSortField;
    property SourceName: widestring read FSourceName write FSourceName;
  end;

  TPivotTable = class(TCollectionItem)
  private
    FTableName: wideString;
    FPivotFields: TCollection;
    FWorksheetName: string;
    function GetPivotField(Index: Integer): TPivotField;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property PivotField[Index: Integer]: TPivotField read GetPivotField;
    property WorksheetName: string read FWorksheetName write FWorksheetName;
  published
    property TableName: wideString read FTableName write FTableName;
    property PivotFields: TCollection read FPivotFields;
  end;

  TPivotGear = class(TComponent)
  private
    FPivotCaches: TCollection;
    FPivotTables: TCollection;
    function GetPivotCache(Index: Integer): TPivotCache;
    function GetPivotTable(Index: Integer): TPivotTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PivotCache[Index: Integer]: TPivotCache read GetPivotCache;
    property PivotTable[Index: Integer]: TPivotTable read GetPivotTable;
  published
    property PivotCaches: TCollection read FPivotCaches;
    property PivotTables: TCollection read FPivotTables;
  end;

implementation

constructor TPivotField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPivotItems := TCollection.Create(TPivotItem);
end;

destructor TPivotField.Destroy;
begin
  FreeAndNil(FPivotItems);
  inherited Destroy;
end;

{
********************************* TPivotField **********************************
}

function TPivotField.GetPivotItem(Index: Integer): TPivotItem;
begin

  result := (self.FPivotItems.Items[Index] as TPivotItem);
end;

procedure TPivotField.SetAutoShowField(const Value: WideString);
begin
  FAutoShowField := Value;
end;

{
********************************* TPivotTable **********************************
}

constructor TPivotTable.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPivotFields := TCollection.Create(TPivotField);
end;

destructor TPivotTable.Destroy;
begin
  FreeAndNil(FPivotFields);
  inherited Destroy;
end;

function TPivotTable.GetPivotField(Index: Integer): TPivotField;
begin
  result := (self.PivotFields.Items[Index] as TPivotField);
end;

{
********************************** TPivotGear **********************************
}

constructor TPivotGear.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPivotTables := TCollection.Create(TPivotTable);
  FPivotCaches := TCollection.Create(TPivotCache);
end;

destructor TPivotGear.Destroy;
begin
  FreeAndNil(FPivotTables);
  FreeAndNil(FPivotCaches);
  inherited Destroy;
end;

function TPivotGear.GetPivotCache(Index: Integer): TPivotCache;
begin
  Result := (FPivotCaches.items[Index] as TPivotCache); ;
end;

function TPivotGear.GetPivotTable(Index: Integer): TPivotTable;
begin

  result := (FPivotTables.Items[Index] as TPivotTable);
end;

end.
