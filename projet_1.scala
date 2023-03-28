// Importation de bibliothèques :
import scala.io.Source
// Lecture de fichiers
import scala.math.{sqrt,pow}
// Effectuer des calculs mathématiques : racine carré et puissance

class IrisData {
    private val columnNames = Array(
        "Sepal.Length", 
        "Sepal.Width", 
        "Petal.Length", 
        "Petal.Width")
    // Un tableau de chaînes de caractères représentant 
    // le nom des colonnes du dataset iris.data
    private var data: Array[Array[Double]] = Array.empty
    // Une matrice stockant les données du fichier iris.data
    private var means: Array[Double] = Array.empty
    // Un tableau de double stockant les moyennes
    private var variances: Array[Double] = Array.empty
    // Un tableau de double stockant les variances
    private var stdDevs: Array[Double] = Array.empty
    // un tableau de double stockant les écarts-types
    private var covariances: Array[Double] = Array.empty
    // Un tableau de double stockant les covariances
    private var nbColumns = 4
    // Le nombre de colonnes qui seront enregistrées dans la matrice
    
    def getData(): Array[Array[Double]] ={
        // Cette méthode affiche le tableau data
        data
    }
    
    def readData(filename: String): Unit = {
        // Cette méthode lit les données à partir du nom de fichier 
        // intégré dans la variable filename et stocke les valeurs 
        // de type double dans la matrice data
        val lines = scala.io.Source.fromFile(filename).getLines.toArray
        // Créer lines, un tableau de string intégrant les lignes du fichier
        data = Array.ofDim[Double](lines.length, nbColumns)
        // Création de data, un tableau de double de dimension (lines.length, nbColumns) 
        for (i <- 0 until lines.length) {
            // Pour tout i de lines.length
            val fields = lines(i).split(",")
            // Tokeniser les i de lines pour les intégrer 
            // dans un tableau fields
            for (j <- 0 until 4) {
                // Pour tout j de 0 à 3
                data(i)(j) = fields(j).toDouble
                // Convertir en double et enregistrer les j fields dans les i,j data
            }
        }
    }
    
    def printData(): Unit = {
        // Afficher les données stockées dans la matrice data
        for (i <- 0 until data.length) {
            // Pour tout i de data.length
            for (j <- 0 until data(i).length) {
                // Pour tout j de data(i).length
                print(data(i)(j) + " ")
                // Afficher les i, j de data
            }
            println()
            // Afficher un retour à la ligne
        }
    }
    
    def calculateMeans(): Unit = {
        // Calculer la moyenne de chaque colonne et stocker 
        // les valeurs dans un tableau dénommé means
        means = Array.ofDim[Double](4)
        // Créer un tableau de double de taille 4
        for (j <- 0 until 4) {
            // Pour tout j de 0 à 3
            var sum = 0.0
            // Initialiser sum à la valeur 0.0
            for (i <- 0 until data.length) {
                // Pour tout i de data.length
                sum += data(i)(j)
                // Faire la somme des i, j de data
            }
            means(j) = sum / data.length
            // Enregistrer les j means : le résultat de sum divisé par data.length
        }
    }
    
    def calculateVariances(): Unit = {
        // Calculer la variance de chaque colonne et stocker 
        // les valeurs dans un tableau dénommé variances
        variances = Array.ofDim[Double](4)
        // Créer variances un tableau de double de taille 4 
        for (j <- 0 until 4) {
            // Pour tout j allant de 0 à 3
            var sum = 0.0
            // Initialiser sum à la valeur 0.0
            for (i <- 0 until data.length) {
                // Pour tout i de data.length
                sum += pow(data(i)(j) - means(j), 2)
                // Calculer sum : la somme des carrés des i, j de data soustraient 
                // par j de means
            }
            variances(j) = sum / data.length
            // Enregistrer les j variances : le résultat de sum divisé par data length
        }
    }
    
    def calculateStdDevs(): Unit = {
        // Calculer l'écart type de chaque colonne et stocker 
        // les valeurs dans un tableau dénommé stdDevs
        stdDevs = Array.ofDim[Double](4)
        // Créer stdDevs un tableau de double de taille 4 
        for (j <- 0 until 4) {
            // Pour tout j allant de 0 à 3
            stdDevs(j) = sqrt(variances(j))
            // Enregistrer les j stdDevs : la racine carrée des j variances
        }
    }


    def printMeans(): Unit = {
        // Afficher la moyenne de chaque colonne :
        for (i <- 0 until means.length) {
            // Pour tout i de means.length
            println(f"Means of ${columnNames(i)}: ${means(i)}%1.2f")
            // Afficher les moyennes
        }
    }
    

    def printVariances(): Unit = {
        // Afficher la variance de chaque colonne : 
        for (i <- 0 until variances.length){
            // Pour tout i de variances.length
            println(f"Variances of ${columnNames(i)} : ${variances(i)}%1.2f")
            // Afficher les variances
        }
    }  
    

    def printStdDevs(): Unit = {
        // Afficher l'écart-type de chaque colonne :
        for (i <- 0 until stdDevs.length){
            // Pour tout i de stdDevs.length
            println(f"StdDevs of ${columnNames(i)} : ${stdDevs(i)}%1.2f")
            // Afficher les écarts-types
        }
    }
    
    def printMinColumn(){
        // Afficher le minimum de chaque colonne :
        for (i <-0 until 4){
            // Pour tout i de 0 à 3
            println(s"${i} : ${iris.getData().map(_(i)).min}")
            // Afficher les valeurs minimums
        }
    }
    
    def printMaxColumn(){
        // Afficher le maximum de chaque colonne :
        for (i <-0 until 4){
            // Pour tout i de 0 à 3
            println(s"${i} : ${iris.getData().map(_(i)).max}")
            // Afficher les valeurs maximums
        }
    } 
    
    def printCorrelations(): Unit = {
        // Afficher les coefficients de corrélation
        covariances = Array.ofDim[Double](4 * 4)
        // Créer covariances un tableau de double de dimension (4, 4)
        for (i <- 0 until 4) {
            // Pour tout i de 0 à 3
            for (j <- 0 until 4) {
                // Pour tout j de 0 à 3
                if (i != j) {
                    // Si i est différent de j
                    var sum = 0.0
                    // Initialiser sum à la valeur 0.0
                    for (k <- 0 until data.length) {
                        // Pour tout k de data.length
                        sum += (data(k)(i) - means(i)) * (data(k)(j) - means(j))
                        // Enregistrer sum : le résultat des k, i de data - 
                        // les i de means multiplié par les k, j de data - 
                        // les j de means
                    }
                    covariances(i * 4 + j) = sum / data.length
                    // Enregistrer les i, j  covariances : le résultat de sum 
                    // divisé par data.length
                    covariances(j * 4 + i) = covariances(i * 4 + j)
                    // Enregistrer les j, i covariances égale aux i, j covariances
                }
            }
        }
        
        for (i <- 0 until 4) {
            // Pour tout i de 0 à 3
            for (j <- 0 until 4) {
                // Pour tout j de 0 à 3
                if (i != j) {
                    // Si i est différent de j
                    val correlation = covariances(i * 4 + j) / (stdDevs(i) * stdDevs(j))
                    // Enregistrer correlation = les i, j covariances divisées par 
                    // les i stdDevs multipliés par les j stdDevs
                    println("Correlation between variable :")
                    println(s"${columnNames(i)} and ${columnNames(j)}: $correlation")
                    // Afficher les corrélations
                }
            }
        }
    }
}

// Exemple d'utilisation de la classe :
val iris = new IrisData()
// Créer une instance de la classe IrisData()
iris.readData("iris.data")
// Exécuter la méthode readData

iris.calculateMeans()
iris.printMeans()
// Affichage des moyennes :

iris.calculateVariances()
iris.printVariances()
// Affichage des variances :

iris.calculateStdDevs()
iris.printStdDevs()
// Affichage des écart-types :

iris.printCorrelations()
// Affichage des coefficients de corrélation :

iris.printData()

// Pour chaque colonne :
// Sepal.length
// Sepal.width
// Petal.length
// Petal.width


//Afficher les données présentes dans data :

iris.printMinColumn()

// Pour chaque colonne :
// 0 : Sepal.length
// 1 : Sepal.width
// 2 : Petal.length
// 3 : Petal.width

// Afficher le minimum de chaque colonne :

iris.printMaxColumn()

// Pour chaque colonne :
// 0 : Sepal.length
// 1 : Sepal.width
// 2 : Petal.length
// 3 : Petal.width

// Afficher le maximum de chaque colonne :


