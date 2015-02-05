# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new(rotations([[0,0]]), board)
  end

  All_My_Pieces = All_Pieces +
                  [rotations([[0,0],[0,-1],[1,0],[1,-1],[2,0]]), # block and extra
                   rotations([[-2,0],[-1,0],[0,0],[1,0],[2,0]]), # long 5
                   rotations([[0,0],[0,-1],[1,0]])] # glider
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_on = false
  end

  # rotates the current piece clockwise
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if @score >= 100
        if !@cheat_on
            @score -= 100
            @cheat_on = true
        end
    end
  end

  # gets the next piece
  def next_piece
    if @cheat_on
        @current_block = MyPiece.cheat_piece(self)
        @cheat_on = false
    else
        @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here

  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    key_bindings_extra
    buttons
    run_game
  end

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings_extra
    # my additional bindings
    @root.bind('u' , proc {@board.rotate_180})
    @root.bind('c' , proc {@board.cheat})
  end

end
