unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  AWS.LexRuntimeV2, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    mmChat: TMemo;
    edMessage: TEdit;
    Send: TButton;
    tsLog: TTabSheet;
    mmLog: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SendClick(Sender: TObject);
  private const
    BotId = 'XXXXXXX';
    BotAliasId = 'XXXXXX';
    LocaleId = 'en_US';
  private
    FClient: IAmazonLexRuntimeV2;
    FSessionId: string;
    function Client: IAmazonLexRuntimeV2;
    procedure StartChat;
    function GetRecognizeTextV2Response(const BotId, BotAliasId, LocaleId,
      SessionId, UserInput: string): IRecognizeTextResponse;
    procedure AddToChat(const Msg: string);
    procedure LogResponse(Response: IRecognizeTextResponse);
//    procedure Log(const Msg: string);
    procedure OrderFullfilled(Intent: TIntent);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddToChat(const Msg: string);
begin
  mmChat.Lines.Add(Msg);
  mmChat.ScrollBy(0, mmChat.Lines.Count);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StartChat;
  edMessage.SetFocus;
end;

function TForm1.Client: IAmazonLexRuntimeV2;
begin
  if FClient = nil then
    FClient := TAmazonLexRuntimeV2Client.Create;
  Result := FClient;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StartChat;
end;

function TForm1.GetRecognizeTextV2Response(const BotId, BotAliasId, LocaleId,
  SessionId, UserInput: string): IRecognizeTextResponse;
var
  Request: IRecognizeTextRequest;
begin
  Request := TRecognizeTextRequest.Create;
  Request.BotAliasId := BotAliasId;
  Request.BotId := BotId;
  Request.LocaleId := LocaleId;
  Request.SessionId := SessionId;
  Request.Text := UserInput;
  Result := Client.RecognizeText(Request);
  LogResponse(Result);
end;

//procedure TForm1.Log(const Msg: string);
//begin
//  mmLog.Lines.Add(Msg);
//  mmLog.ScrollBy(0, mmLog.Lines.Count);
//end;

procedure TForm1.LogResponse(Response: IRecognizeTextResponse);
begin
end;

procedure TForm1.OrderFullfilled(Intent: TIntent);
var
  SlotValue: string;
  Slot: TPair<string, TSlot>;
begin
  AddToChat('System: Your order will be processed with following information:');
  for Slot in Intent.Slots do
  begin
    if (Slot.Value <> nil) and (Slot.Value.Value <> nil) then
      SlotValue := Slot.Value.Value.InterpretedValue
    else
      SlotValue := '(not specified)';
    AddToChat(Format('%s: %s', [Slot.Key, SlotValue]));
  end;
end;

procedure TForm1.SendClick(Sender: TObject);
var
  UserInput: string;
  Response: IRecognizeTextResponse;
  Msg: TMessage;
  LocalBotId: string;
  LocalBotAliasId: string;
begin
  UserInput := edMessage.Text;
  AddToChat('You: ' + UserInput);
  edMessage.Text := '';
  Application.ProcessMessages;

  LocalBotId := GetEnvironmentVariable('AWS_DELPHI_SAMPLES_BOTID');
  if LocalBotId = '' then
    LocalBotId := BotId;

  LocalBotAliasId := GetEnvironmentVariable('AWS_DELPHI_SAMPLES_BOTALIASID');
  if LocalBotAliasId = '' then
    LocalBotAliasId := BotAliasId;

  Response := GetRecognizeTextV2Response(LocalBotId, LocalBotAliasId, LocaleId, FSessionId, UserInput);
  for Msg in Response.Messages do
    AddToChat('Bot: ' + Msg.Content);
  if Response.IsSetSessionStateValue and Response.SessionStateValue.IsSetIntent
    and (Response.SessionStateValue.Intent.ConfirmationState = TConfirmationState.Confirmed) then
    OrderFullfilled(Response.SessionStateValue.Intent);
end;

procedure TForm1.StartChat;
begin
  FSessionId := TGuid.NewGuid.ToString.Substring(1, 36);
  mmChat.Lines.Clear;
end;

end.
