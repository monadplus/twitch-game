## Back end

The server runs on port `8080` (running it on a different port is a todo).

### Endpoints

This service is really coupled to the game and it was not modeled to be game-independant.

Replace your host for:

- `localhost` for local development.
- `aws url` for production.

### Create game

```bash
$ curl -XPUT host:8080/game -H "Content-Type: application/json" -d '{ "channel": "otter_chaos_repair", "commands": [ "paint", "tape", "fish", "shell"] }'

{ "session_id": "xxx-yyyy-zzzz-www" }
```

### Update Frame

```bash
curl -X GET host:8080/game/<session_id> -H "Content-Type: application/json"

Returns a data frame structure
```

### End game

```bash
curl -X DELETE host:8080/game/<session_id> -H "Content-Type: application/json"
```
