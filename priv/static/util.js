var suit2str = {
    'spade': '\u2660',
    'heart': '\u2665',
    'diamond': '\u2666',
    'club': '\u2663'
};
var rank2str = {
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'nine': '9',
    'ten': 'T',
    'jack': 'J',
    'queen': 'Q',
    'king': 'K',
    'ace': 'A'
};

function card2str(card) {
    if (card == 'unknown') return '?';
    return rank2str[card['rank']] + suit2str[card['suit']];
}

function cards2str(cards) {
    return cards.map(card2str).join(' ');
}

