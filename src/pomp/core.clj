(ns pomp.core
  (:gen-class))

(def token-colors [:blue :orange :purple :yellow :red :gold])

(def location-tiles
  [{:name "Nowhere" :points 3 :cost {:yellow 3 :blue 3 :red 3}}
   {:name "Atlantis" :points 3 :cost {:blue 3 :red :3 :purple 3}}
   {:name "Avengers Tower, NYC" :points 3 :cost {:yellow 4 :blue 4}}
   {:name "Attilan" :points 3 :cost {:red 4 :purple 4}}
   {:name "Asgard" :points 3 :cost {:yellow 3 :orange 3 :purple 3}}
   {:name "Wakanda" :points 3 :cost {:blue 4 :red 4}}
   {:name "Hell's Kitchen, NYC" :points 3 :cost {:yellow 4 :orange 4}}
   {:name "Triskelion" :points 3 :cost {:orange 4 :purple 4}}])

(def character-cards
  [{:name "Spider-Man 2099" :level 1 :color :blue :cost {:yellow 2 :red 1}}
   {:name "Abomination" :level 1 :color :purple :cost {:blue 2 :orange 1 :red 1 :purple 1}}
   {:name "Rocket" :level 1 :color :yellow :cost {:yellow 1 :red 2}}
   {:name "Kate Bishop" :level 1 :color :orange :avenger-tags 1 :cost {:blue 1 :red 1 :purple 3}}
   {:name "Prowler" :level 1 :color :yellow :cost {:blue 1 :orange 1 :red 2 :purple 1}}
   {:name "War Machine" :points 1 :level 2 :color :blue :avenger-tags 1 :cost {:blue 2 :orange 2 :purple 3}}
   {:name "Cossbones" :points 2 :level 2 :color :purple :cost {:yellow 4 :blue 2 :orange 1}}
   {:name "She-Hulk" :points 1 :level 2 :color :purple :avenger-tags 1 :cost {:yellow 3 :orange 2 :purple 2}}
   {:name "Red Skull" :points 3 :level 2 :color :yellow :cost {:orange 6}}
   {:name "Nick Fury" :points 1 :level 2 :color :orange :avenger-tags 1 :cost {:orange 2 :red 3 :purple 2}}
   {:name "Gamora" :points 5 :level 3 :color :blue :cost {:orange 3 :red 7}}
   {:name "Vision" :points 4 :level 3 :color :yellow :avenger-tags 1 :cost {:yellow 3 :blue 6 :red 3}}
   {:name "Luke Cage" :points 5 :level 3 :color :purple :cost {:yellow 3 :orange 7}}
   {:name "Thor" :points 3 :level 3 :color :red :avenger-tags 2 :cost {:yellow 3 :blue 3 :red 3 :purple 5}}
   {:name "Loki" :points 5 :level 3 :color :red :cost {:blue 3 :purple 7}}])

(defn pick-location-tiles [tiles num-players] (take num-players (shuffle tiles)))

(defn setup-market-level [cards level]
  (let [level-character-cards (filter #(= level (:level %)) cards)]
    {:deck (drop 4 level-character-cards) :table (take 4 level-character-cards)}))

(defn create-market [cards]
  (let [shuffled-character-cards (shuffle cards)]
    [(setup-market-level shuffled-character-cards 1)
     (setup-market-level shuffled-character-cards 2)
     (setup-market-level shuffled-character-cards 3)]))

(defn calc-normal-token-amount [num-players]
  (case num-players 2 4, 3 5, 4 7))

(defn special-token? [color] (= color :gold))

(defn calc-token-amount [color num-players]
  (if (special-token? color) 5
  (calc-normal-token-amount num-players)))

(defn create-token-area [colors num-players]
  (zipmap colors (map #(calc-token-amount % num-players) colors)))

(defn create-player [] {:id (random-uuid) :hand [] :play-area [] :tiles [] :tokens {}})

(defn create-game [num-players]
  (let [tiles (pick-location-tiles location-tiles num-players)
        market (create-market character-cards)
        tokens (create-token-area token-colors num-players)
        players (repeatedly num-players create-player)]
    {:tiles tiles
     :market market
     :tokens tokens
     :players players
     :turn 0}))

(defn- deduct-tokens-from-pool [game colors]
  (assoc game :tokens (reduce (fn [t c] (update t c dec)) (:tokens game) colors)))

(defn- add-tokens-to-player [player colors]
  (assoc player :tokens (reduce (fn [t c] (update t c (fnil inc 0))) (:tokens player) colors)))

(defn- add-tokens-to-current-player [game colors]
  (let [updated-player (add-tokens-to-player (first (:players game)) colors)]
  (assoc game :players (cons updated-player (rest (:players game))))))

(defn- advance-turn [game]
  (-> game
      (update :turn inc)
      (update :players #(concat (rest %) (list (first %))))))

(defn- try-advance-turn [game]
  (let [player (first (:players game))]
    (if (> (reduce + 0 (vals (:tokens player))) 10)
      game
      (advance-turn game))))

(defn take-tokens [game colors]
    (-> game
      (deduct-tokens-from-pool colors)
      (add-tokens-to-current-player colors)
      try-advance-turn))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
