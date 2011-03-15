Unit
	LoggerActor;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	SyncObjs,
	ContNrs,
	Actors,
	EventLog;

Const
	ccDefaultLogger = 'logger';

Type
	TLogCustomMessage = Class(TCustomMessage)
	Private
		fMessage : String;
	Public
		Constructor Create(Const aSender : String = ''; Const aReceiver : String = ''; Const aMessage : String = ''); Overload;
		Property LogMessage : String Read fMessage Write fMessage;
	End;
	
	TLogInfoMessage = Class(TLogCustomMessage);
	TLogWarningMessage = Class(TLogCustomMessage);
	TLogErrorMessage = Class(TLogCustomMessage);
	TLogDebugMessage = Class(TLogCustomMessage);
	
	TLoggerActor = Class(TActorThread)
	Private
		fLog : TEventLog;
	Public
		Constructor Create(Const aName : String = ccDefaultLogger; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure Info(Var aMessage); Message 'tloginfomessage';
		Procedure Warning(Var aMessage); Message 'tlogwarningmessage';
		Procedure Error(Var aMessage); Message 'tlogerrormessage';
		Procedure Debug(Var aMessage); Message 'tlogdebugmessage';
		Procedure DefaultHandlerStr(Var aMessage); Override;
	End;

Implementation

// TLogCustomMessage

Constructor TLogCustomMessage.Create(Const aSender : String = ''; Const aReceiver : String = ''; Const aMessage : String = '');
Begin
	Inherited Create(aSender, aReceiver);
	fMessage := aMessage;
End;

// TLoggerActor

Constructor TLoggerActor.Create(Const aName : String = ccDefaultLogger; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	fLog := TEventLog.Create(Nil);
End;

Destructor TLoggerActor.Destroy;
Begin
	fLog.Free;
	Inherited Destroy;
End;

Procedure TLoggerActor.Idle;
Begin
End;

Procedure TLoggerActor.Info(Var aMessage);
Begin
	fLog.Info((UnbundleMessage(aMessage) As TLogInfoMessage).LogMessage);
End;

Procedure TLoggerActor.Warning(Var aMessage);
Begin
	fLog.Warning((UnbundleMessage(aMessage) As TLogWarningMessage).LogMessage);
End;

Procedure TLoggerActor.Error(Var aMessage);
Begin
	fLog.Error((UnbundleMessage(aMessage) As TLogErrorMessage).LogMessage);
End;

Procedure TLoggerActor.Debug(Var aMessage);
Begin
	fLog.Debug((UnbundleMessage(aMessage) As TLogDebugMessage).LogMessage);
End;

Procedure TLoggerActor.DefaultHandlerStr(Var aMessage);
Begin
	fLog.Error('Received a message of unknown class : ' + UnbundleMessage(aMessage).ClassName);
End;

End.