Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	Actors,
	ActorMessages,
	ActorLogger,
	CustomActors;

Type
	TScreenMessage = Class(TCustomStringActorMessage);

	TScreenWriterActor = Class(TActorThread)
	Public
		Procedure ScreenWrite(Var aMessage); Message 'tscreenmessage';
	End;

Procedure TScreenWriterActor.ScreenWrite(Var aMessage);
Var
	lMessage : TScreenMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TScreenMessage;
	WriteLn(ActorName, ': ', lMessage.Data);
End;

Var
	gBuffer : String;
	gScreenMessage : TScreenMessage;

Begin
	DefaultActorMessageTTL := 1000000; // things are rough at startup, lets make messages float around forever
	Actors.Init('localhost', 'switchboard');
	ActorLogger.Init;
	CustomActors.Init;
	ActorMessageClassFactory.RegisterMessage(TScreenMessage);
	RegisterActorClass(TScreenWriterActor);
	StartActorInstance('TScreenWriterActor', 'screen1');
	StartActorInstance('TScreenWriterActor', 'screen2');
	StartActorInstance('TScreenWriterActor', 'screen3');
	StartActorInstance('TLoadBalancerActor', 'screen');
	AddTargetToActor('screen', 'screen1');
	AddTargetToActor('screen', 'screen2');
	AddTargetToActor('screen', 'screen3');
	DefaultActorMessageTTL := 5; // everything should be quick from now on.
	Repeat
		Write('Input something : '); ReadLn(gBuffer);
		If gBuffer <> 'quit' Then
		Begin
			gScreenMessage := TScreenMessage.Create(ccDefaultMainThreadName, 'screen');
			gScreenMessage.Data := gBuffer;
			Switchboard.Mailbox.Push(gScreenMessage);
		End;
	Until gBuffer = 'quit';
	CustomActors.Fini;
	ActorLogger.Fini;
	Actors.Fini;
End.