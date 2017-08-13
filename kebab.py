from requests import Session
from signalr import Connection

MARKET_ARRAY = ['BTC-DGB', 'BTC-STRAT', 'CFC-APX']

with Session() as session:
    # create a connection
    connection = Connection("https://socket.bittrex.com/signalr", session)

    # get chat hub
    chat = connection.register_hub('coreHub')

    connection.start()

    # create new chat message handler
    def print_received_message(data):
        if data['Nounce'] is not None:
            print('Nounce', data['Nounce'])

        if data['Deltas'] is not None:
            for value in data['Deltas']:
                print(value)

    # create error handler
    def print_error(error):
        print('error: ', error)

    # debug information, show all data
    # def print_raw_data(*args, **kwargs):
    #    print (args, kwargs)
    # connection.received += print_raw_data

    # receive new chat messages from the hub
    chat.client.on('updateSummaryState', print_received_message)

    # process errors
    connection.error += print_error

    with connection:
        chat.server.invoke('querySummaryState', MARKET_ARRAY)

while True:
    pass