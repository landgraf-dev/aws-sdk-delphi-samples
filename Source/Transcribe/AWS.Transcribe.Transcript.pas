unit AWS.Transcribe.Transcript;

interface

uses
  System.SysUtils,
  System.JSON,
  Generics.Collections,
  AWS.Transcribe;

type
  TTranscript = class;

  TTrancriptObject = class abstract
  strict private
    FTranscript: TTranscript;
  protected
    procedure LoadObject(const AValue: TJSONObject); virtual; abstract;
  public
    constructor Create(ATranscript: TTranscript; const AValue: TJSONObject);
    property Transcript: TTranscript read FTranscript;
  end;

  TTrancriptRedaction = class (TTrancriptObject)
  private
    FRedactionType: TPiiEntityType;
    FConfidence: Single;
    FCategory: TRedactionType;
  protected
    procedure LoadObject(const AValue: TJSONObject); override;
  public
    property Confidence: Single read FConfidence;
    property RedactionType: TPiiEntityType read FRedactionType;
    property Category: TRedactionType read FCategory;
  end;

  TTrancriptAlternative = class (TTrancriptObject)
  private
    FConfidence: Single;
    FContent: string;
    FRedactions: TList<TTrancriptRedaction>;
    function GetRedacted: Boolean;
  protected
    procedure LoadObject(const AValue: TJSONObject); override;
  public
    destructor Destroy; override;
    property Confidence: Single read FConfidence;
    property Content: string read FContent;
    property Redactions: TList<TTrancriptRedaction> read FRedactions;
    property Redacted: Boolean read GetRedacted;
  end;

  TTranscriptSegment = class;

  TTrancriptType = (pronunciation, punctuation);

  TTranscriptItem = class (TTrancriptObject)
  strict private
    FStartTime: Single;
    FEndTime: Single;
    FAlternatives: TList<TTrancriptAlternative>;
    FItemType: TTrancriptType;
    FSegment: TTranscriptSegment;
  private
    function TrancriptType(const AValue: string): TTrancriptType;
  protected
    procedure LoadObject(const AValue: TJSONObject); override;
  public
    destructor Destroy; override;
    property StartTime: Single read FStartTime;
    property EndTime: Single read FEndTime;
    property Alternatives: TList<TTrancriptAlternative> read FAlternatives;
    property ItemType: TTrancriptType read FItemType;
    property Segment: TTranscriptSegment read FSegment write FSegment;
  end;

  TTranscriptSegment = class (TTrancriptObject)
  strict private
    FStartTime: Single;
    FItems: TList<TTranscriptItem>;
    FEndTime: Single;
    FSpeakerLabel: string;
  protected
    procedure LoadObject(const AValue: TJSONObject); override;
  public
    destructor Destroy; override;
    property StartTime: Single read FStartTime;
    property EndTime: Single read FEndTime;
    property SpeakerLabel: string read FSpeakerLabel;
    property Items: TList<TTranscriptItem> read FItems;
  end;

  TTranscript = class
  strict private
    FJobName: string;
    FIsRedacted: Boolean;
    FSpeakers: Integer;
    FTexts: TArray<string>;
    FItems: TList<TTranscriptItem>;
    FSegments: TList<TTranscriptSegment>;
    function GetTexts(const AIndex: Integer): string;
    function GetItems(const AIndex: Integer): TTranscriptItem;
    function GetTextCount: Integer;
    function GetItemCount: Integer;
  private
    procedure LoadTranscript(const AJson: string);
    function GetSegmentCount: Integer;
    function GetSegments(const AIndex: Integer): TTranscriptSegment;
  public
    constructor Create(const AJson: string);
    destructor Destroy; override;
    function ItemAt(const AStartTime: Single): Integer;
    property JobName: string read FJobName;
    property IsRedacted: Boolean read FIsRedacted;
    property Speakers: Integer read FSpeakers;
    property Texts[const AIndex: Integer]: string read GetTexts; default;
    property Items[const AIndex: Integer]: TTranscriptItem read GetItems;
    property Segments[const AIndex: Integer]: TTranscriptSegment read GetSegments;
    property TextCount: Integer read GetTextCount;
    property ItemCount: Integer read GetItemCount;
    property SegmentCount: Integer read GetSegmentCount;
  end;

implementation

uses
  System.TypInfo;

{ TTrancriptObject }

constructor TTrancriptObject.Create(ATranscript: TTranscript; const AValue: TJSONObject);
begin
  inherited Create;
  FTranscript := ATranscript;
  LoadObject(AValue);
end;

{ TTrancriptRedaction }

procedure TTrancriptRedaction.LoadObject(const AValue: TJSONObject);
begin
  FCategory := TRedactionType.PII; {'category'}
  FRedactionType := AValue.GetValue<string>('type');
  FConfidence := AValue.GetValue<Single>('confidence');
end;

{ TTrancriptAlternative }

procedure TTrancriptAlternative.LoadObject(const AValue: TJSONObject);
var
  I: Integer;
  A: TJsonArray;
begin
  FContent := AValue.GetValue<string>('content');
  if Redacted then
  begin
    FRedactions := TObjectList<TTrancriptRedaction>.Create(True);
    A := AValue.FindValue('redactions') as TJsonArray;
    for I := 0 to A.Count-1 do
      FRedactions.Add(TTrancriptRedaction.Create(Transcript, A.Items[I] as TJsonObject));
    FConfidence := 0.0; // not applied
  end else
    FConfidence := AValue.GetValue<Single>('confidence');
end;

destructor TTrancriptAlternative.Destroy;
begin
  if Redacted then
    FRedactions.Free;
  inherited;
end;

function TTrancriptAlternative.GetRedacted: Boolean;
begin
  Result := FContent='[PII]';
end;

{ TTranscriptItem }

procedure TTranscriptItem.LoadObject(const AValue: TJSONObject);
var
  I: Integer;
  A: TJsonArray;
begin
  FAlternatives := TObjectList<TTrancriptAlternative>.Create(True);
  FSegment := nil;
  FItemType := TrancriptType(AValue.GetValue<string>('type'));
  if FItemType=TTrancriptType.pronunciation then
  begin
    FStartTime := AValue.GetValue<Single>('start_time');
    FEndTime := AValue.GetValue<Single>('end_time');
  end else
  begin
    FStartTime := 0; // not applied
    FEndTime := 0;   // not applied
  end;
  A := AValue.FindValue('alternatives') as TJsonArray;
  for I := 0 to A.Count-1 do
    FAlternatives.Add(TTrancriptAlternative.Create(Transcript, A.Items[I] as TJsonObject));
end;

destructor TTranscriptItem.Destroy;
begin
  FAlternatives.Free;
  inherited;
end;

function TTranscriptItem.TrancriptType(const AValue: string): TTrancriptType;
begin
  Result := TTrancriptType(GetEnumValue(TypeInfo(TTrancriptType), AValue));
end;

{ TTranscriptSegment }

procedure TTranscriptSegment.LoadObject(const AValue: TJSONObject);
var
  I: Integer;
  J: Integer;
  A: TJsonArray;
  S: Single;
  E: Single;
  L: string;
  O: TTranscriptItem;
begin
  FItems := TList<TTranscriptItem>.Create;
  FStartTime := AValue.GetValue<Single>('start_time');
  FEndTime := AValue.GetValue<Single>('end_time');
  FSpeakerLabel := AValue.GetValue<string>('speaker_label');

  A := AValue.FindValue('items') as TJSONArray;
  for I := 0 to A.Count-1 do
  begin
    S := A.Items[I].GetValue<Single>('start_time');
    E := A.Items[I].GetValue<Single>('end_time');
    L := A.Items[I].GetValue<string>('speaker_label');
    J := Transcript.ItemAt(S);
    if J>=0 then
    repeat
      O := Transcript.Items[J];
      if (O.StartTime=S) and (O.EndTime=E) and (O.Segment=nil) then
      begin
        O.Segment := Self;
        FItems.Add(O);
        if (J+1<Transcript.ItemCount) then
        begin
          O := Transcript.Items[J+1];
          if O.ItemType=TTrancriptType.punctuation then
          begin
            O.Segment := Self;
            FItems.Add(O);
          end;
        end;
        Break;
      end;
      Inc(J);
    until (J>=Transcript.ItemCount) or (Transcript.Items[J].StartTime>S);
  end;

end;

destructor TTranscriptSegment.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TTranscript }

constructor TTranscript.Create(const AJson: string);
begin
  inherited Create;
  FSegments := nil;
  FItems := TObjectList<TTranscriptItem>.Create(True);
  LoadTranscript(AJson);
end;

destructor TTranscript.Destroy;
begin
  SetLength(FTexts, 0);
  if Assigned(FSegments) then
    FSegments.Free;
  FItems.Free;
  inherited;
end;

function TTranscript.GetTextCount: Integer;
begin
  Result := Length(FTexts);
end;

function TTranscript.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TTranscript.GetSegmentCount: Integer;
begin
  if Assigned(FSegments) then
    Result := FSegments.Count
  else
    Result := 0; // not applied
end;

function TTranscript.GetTexts(const AIndex: Integer): string;
begin
  if (AIndex<0) or (AIndex>=TextCount) then
    Result := ''
  else
    Result := FTexts[AIndex];
end;

function TTranscript.GetItems(const AIndex: Integer): TTranscriptItem;
begin
  Result := FItems[AIndex];
end;

function TTranscript.GetSegments(const AIndex: Integer): TTranscriptSegment;
begin
  if SegmentCount>0 then
    Result := FSegments[AIndex]
  else
    Result := nil;
end;

function TTranscript.ItemAt(const AStartTime: Single): Integer; {not optimal}
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ItemCount-1 do
    if Items[I].StartTime>=AStartTime then
      Exit(I);
end;

procedure TTranscript.LoadTranscript(const AJson: string);
var
  I: Integer;
  O: TJSONObject;
  L: TJSONValue;
  A: TJsonArray;
begin
  O := TJSONValue.ParseJSONValue(AJson, True) as TJsonObject;
  try
    FJobName := O.GetValue<string>('jobName');
    if not O.TryGetValue<Boolean>('isRedacted', FIsRedacted) then
      FIsRedacted := False;
    FSpeakers := 0; // not applied

    { transcripts }
    A := O.FindValue('results.transcripts') as TJsonArray;
    if (A<>nil) and (A.Count>0) then
    begin
      SetLength(FTexts, A.Count);
      for I := 0 to A.Count-1 do
        FTexts[I] := A.Items[I].GetValue<string>('transcript');
    end else
      raise Exception.Create('Empty transcript file');

    { items }
    A := O.FindValue('results.items') as TJsonArray;
    if (A<>nil) and (A.Count>0) then
    begin
      for I := 0 to A.Count-1 do
        FItems.Add(TTranscriptItem.Create(Self, A.Items[I] as TJSONObject));
    end else
      raise Exception.Create('Empty transcript file');

    { segments }
    L := O.FindValue('results.speaker_labels');
    if L<>nil then
    begin
      FSpeakers := L.GetValue<Integer>('speakers');
      A := L.FindValue('segments') as TJsonArray;
      if (A<>nil) and (A.Count>0) then
      begin
        FSegments := TObjectList<TTranscriptSegment>.Create(True);
        for I := 0 to A.Count-1 do
          FSegments.Add(TTranscriptSegment.Create(Self, A.Items[I] as TJSONObject));
      end;
    end;

  finally
    O.Free;
  end;
end;

end.
