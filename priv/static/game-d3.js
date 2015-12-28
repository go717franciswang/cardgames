
function draw(gameState) {
    var player2nickname = {};
    for (var i = 0; i < gameState['users'].length; i++) {
        var user = gameState['users'][i];
        player2nickname[user['player']] = user['nickname'];
    }

    var width = 200;
    var height = 200;
    var seatCount = gameState['seats'].length;

    var svg = d3.select('#container').selectAll('#game').data([gameState]);
    svg.enter().append('svg').attr('id', 'game').attr('width', width).attr('height', height); 
    var seat = svg.selectAll('g.d3seat').data(function(d) { 
        return d['seats'] 
    });

    var seatEnter = seat.enter().append('g').attr('class', 'd3seat').attr('transform', function(seat) {
        var degree = 2*Math.PI/seatCount*(seat['position']-1);
        var x = Math.cos(degree)*width*0.4+width/2;
        var y = Math.sin(degree)*height*0.4+height/2;
        return 'translate('+x+','+y+')';
    });
    seatEnter.append('text').attr('class', 'nickname');
    seatEnter.append('text').attr('class', 'money').attr('y', 15);
    seat.select('.nickname').text(function(d) { 
        return d['player'] ? player2nickname[d['player']] : '-';
    });
    seat.select('.money').text(function(d) { 
        return d['money'] ? d['money'] : '';
    });

    var cards = seatEnter.append('g').attr('class', 'cards');
    var card = cards.selectAll('text.card').data(function(d) { 
        return d['cards'] ? d['cards'] : [];
    });
    card.enter().append('text').attr('class', 'card');
    card.text(function(d) { return d['rank']+' '+d['suit'] });
    card.exit().remove();
}


