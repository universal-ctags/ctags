# https://www.python.org/dev/peps/pep-0492/

async def read_data(db):
    pass

async def commit(session, data):
    # ...

    async with session.transaction():
        # ...
        await session.update(data)
        # ...

class Cursor:
    def __init__(self):
        self.buffer = collections.deque()

    async def _prefetch(self):
        # ...
        pass

    def __aiter__(self):
        return self

    async def __anext__(self):
        if not self.buffer:
            self.buffer = await self._prefetch()
            if not self.buffer:
                raise StopAsyncIteration
        return self.buffer.popleft()

class AsyncIteratorWrapper:
    def __init__(self, obj):
        self._it = iter(obj)

    def __aiter__(self):
        return self

    async def __anext__(self):
        try:
            value = next(self._it)
        except StopIteration:
            raise StopAsyncIteration
        return value

async for letter in AsyncIteratorWrapper("abc"):
    print(letter)

# async generator from https://www.python.org/dev/peps/pep-0525/
async def ticker(delay, to):
    """Yield numbers from 0 to `to` every `delay` seconds."""
    for i in range(to):
        yield i
        await asyncio.sleep(delay)

# check it doesn't affect decorators parsing
class TestDecorators:
    @staticmethod
    def simplestuff():
        pass

    @staticmethod
    async def something(other):
        await other
