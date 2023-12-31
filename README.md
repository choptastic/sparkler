# Sparkler

An ultra-minimal Sparkpost Mail Sender for Erlang

This is a fork of [mindrill](https://github.com/choptastic/mindrill) for
Erlang, and is mostly API-compatible.

## Configure it

```erlang
[{sparkler, [
	{api_key, "MY_API_KEY"}
]}].
```

## Start The App

Start the app:

```erlang
sparkler:start()
```

## Send mail

```erlang
sparkler:send("from@email.com", "to@email.com", "subject", "this is the message").
```

# License

MIT LICENSE

Copyright &copy; 2016-2023 [Jesse Gumm](http://jessegumm.com) ([@jessegumm](http://twitter.com/jessegumm))
