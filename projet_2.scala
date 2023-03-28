import scala.io.Source
import scala.util.Random


class KMeans(dataFile: String, k: Int, maxIterations: Int) {
    
    private val data: Array[Array[Double]] = readData(dataFile)
    // Initialiser du tableau à deux dimensions data à l'aide des données 
    // du fichier data.iris
    private val nbColumns: Int = 4
    // Initialisation de la variable nbColumns qui va prendre la valeur 4
    private val nbLines = data.length
    // Initialisation de la variable NbLines qui va prendre la valeur 150
    private var centroids: Array[Array[Double]] = initCentroids(k)
    // Initialiser de manière aléatoire k centroïdes distincts
    
    def readData(filename: String): Array[Array[Double]] = {
        // Cette méthode lit les données à partir du nom de fichier 
        // intégré dans la variable filename et stocke les valeurs 
        // de type double dans la matrice data
        val lines = scala.io.Source.fromFile(filename).getLines.toArray
        // Création de lines, un tableau de string intégrant les lignes 
        // du fichier filename
        val data = Array.ofDim[Double](lines.length, 4)
        // Création de data, un tableau de double de dimension (lines.lenght, 4)
        
        for (i <- 0 until lines.length) {
            // Pour i lignes de lines
            val fields = lines(i).split(",")
            // Tokeniser les éléments de tableau lines dans fields
            for (j <- 0 until 4) {
                // Pour i allant de 0 à 3 
                data(i)(j) = fields(j).toDouble
                // Enregistrer fields(j) dans data(i)(j)
            }
        }
        data
        // Retourner data
    }
    
    private def initCentroids(k: Int): Array[Array[Double]] = {
        // Cette méthode initialise aléatoirement k centroïdes
        val centroids = new Array[Array[Double]](k)
        // Créer un tableau de double à deux dimension de dimension (1, k)
        for (i <- 0 until k) {
            // Pour tout i de k
            centroids(i) = data(Random.nextInt(data.length))
            // Enregister i centroïdes dans le tableau centroids
        }
        centroids
        // Retourner centroids
    }
    
    private def euclideanDistance(arr1: Array[Double], arr2: Array[Double]): Double = {
        // Cette méthode calcule la distance euclidienne 
        // entre les deux tableaux arr1 et arr2
        var sum = 0.0
        for (i <- 0 until arr1.length) {
            // Pour tout i de arr1.length
            sum += math.pow(arr1(i) - arr2(i), 2)
            // Somme des i arr1 - les i arr2 au carré
        }
        math.sqrt(sum)
        // Retourner la racine carré de la somme totale
    }
    
    private def assignToClusters(): Array[Int] = {
        // Cette méthode attribue chaque point de données à un centroïde 
        // le plus proche à l'aide de la distance euclidienne pour mesurer 
        // la similarité entre chaque point de données et chaque centroïde
        val clusterAssignments = new Array[Int](data.length)
        for (i <- 0 until data.length) {
            // Pour chaque i de data :
            var minDistance = Double.MaxValue
            // Initialiser minDistance à la valeur maximale
            var closestCenterIndex = -1
            // Initialiser de closestCenterIndex à -1
            for (j <- 0 until centroids.length) {
                // Pour tout j de centroids
                val distance = euclideanDistance(data(i), centroids(j))
                // Calculer la distance euclidienne entre un point d'indice i dans data
                // et un centroïde d'indice j dans centroids
                if (distance < minDistance) {
                    // Si la distance euclidienne est inférieure 
                    // à la distance minimale enregistrée
                    minDistance = distance
                    // alors la distance est enregistrée dans mindistance
                    closestCenterIndex = j
                    // et l'indice j est enregistré dans closestCenterIndex
                }
            }
            clusterAssignments(i) = closestCenterIndex
            // Le tableau clusterAssignments permet d'enregistrer 
            // les indices des centroïdes les plus proches pour chaque i
        }
        clusterAssignments
        // Retourner clusterAssignements
    }
    
    // Calcul des nouveaux centres de chaque cluster
    private def calculateCenters(clusterAssignments: Array[Int]): Array[Array[Double]] = {
        // Cette méthode calcule les nouveaux centres grâce 
        // à la moyenne des données assignées à chaque cluster
        val newCenters = Array.ofDim[Double](k, data(0).length)
        // Création de newCenters, un tableau de double de dimension (k, 4)
        val counts = new Array[Int](k)
        // Création de counts, un tableau d'entier de dimension k
        
        for (i <- data.indices) {
            // Pour tout i de data.length :
            val clusterId = clusterAssignments(i)
            // Récupérer la valeur du cluster enregistrée dans les i clusterAssignments
            for (j <- data(i).indices) {
                // Pour tout j de data(i).length :
                newCenters(clusterId)(j) += data(i)(j)
                // Calculer la somme des i,j data et enregistrer le résultat 
                // dans les clustersId, j de newCenters
                
            }
            counts(clusterId) += 1
            // Enregistrer le nombre de points de chaque cluster
        }
        for (i <- centroids.indices) {
            // Pour tout i de centroids.indices :
            for (j <- centroids(i).indices) {
                // Pour tout j de centroids(i).indices :
                newCenters(i)(j) /= counts(i)
                // Diviser les i, j newCenters par les i counts 
                // et enregistrer le résultat dans les i, j newCenters
            }
        }
        newCenters
        //Retourner newCenters
    }
    
    def run(): Array[Int] = {
        // Cette méthode exécute l'algorithme k-means 
        var clusterIds = assignToClusters()
        // Attribuer chaque point du dataset à un centroïde initialement aléatoirement
        var newCenters = calculateCenters(clusterIds)
        // Calculer le nouveau centroïde
        while (centroids.sameElements(newCenters)) {
            // Tant que centroids est différent de newCenters :
            centroids = newCenters
            // Enregistrer newCenters dans centroids
            clusterIds = assignToClusters()
            // Assigner les points de data au nouveau centroïde
            newCenters = calculateCenters(clusterIds)
            // Calculer de nouveaux centroïdes newCenters
        }
        clusterIds
        // Retourner ClusterIds
    }
    
    def getCentroids(): Array[Array[Double]] ={
        // Assesseur en lecture concernant le tableau centroids
        return centroids
        // Retourner centroids
    }

    def predict(point: Array[Double]): Int = {
        // Cette méthode renvoie l'indice du cluster 
        // auquel un point donné en argument appartient
        var minDistance = Double.MaxValue
        // Initialiser minDistance à la valeur maximumale
        var clusterIndex = -1
        // Initialiser clusterIndex à -1
        for (i <- centroids.indices) {
            // Pour tout i de centroids.indices :
            val distance = euclideanDistance(point, centroids(i))
            // Calculer la distance euclidienne entre le point et les centroïdes
            if (distance < minDistance) {
                // Si la distance est inférience à minDistance
                minDistance = distance
                // minDistance prend la valeur de distance
                clusterIndex = i
                // Ajouter la valeur de i dans clusterIndex
            }
        }
        clusterIndex
        // Retourner clusterIndex
    }
}

val kMeans = new KMeans("iris.data", 3, 1000)
// Créer une instance de la classe KMeans
val clusters = kMeans.run()
// Exécuter la méthode run implémentant l'algorithme K-Means

println(s"Final clusters: ${clusters.mkString(", ")}")
// Afficher les indices des clusters après exécution de K-Means

val cent = kMeans.getCentroids()
// Renvoie les valeurs des centroïdes

val indiceCluster = kMeans.predict(Array[Double](5, 3, 1, 0.5)) 

import $ivy.`org.plotly-scala::plotly-almond:0.8.2`
// Téléchargement la version de 0.8.2 de plotly-scala pour almond 

import plotly._
// Importer tous les sous-modules de Plotly
import plotly.element._
// Importer les définitions pour différents éléments graphiques 
// comme les couleurs, les bordures, etc
import plotly.layout._
// Importer la définition de la mise en page 
// des graphiques : les titres, les légendes, les axes, etc
import plotly.Almond._
// Importer une extension de plotly pour almond sous jupyter notebook

val data = scala.io.Source.fromFile("iris.data").getLines.map(_.split(",")).toSeq
// Tokenisation des lignes du fichier iris.data : 

val irisSetosaCount = data.filter(_(4) == "Iris-setosa").size
// Compter le nombre de lignes contenant la chaîne de caractères : "Iris-setosa"
val irisVersicolorCount = data.filter(_(4) == "Iris-versicolor").size
// Compter le nombre de lignes contenant la chaîne de caractères : "Iris-versicolor"
val irisVirginicaCount = data.filter(_(4) == "Iris-virginica").size
// Compter le nombre de lignes contenant la chaîne de caractères : "Iris-virginica"

val data_iris = Seq(
  Bar(
      // Créer une séquence de barres
    Seq("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
      // pour Iris-sesota, Iris-versicolor, Iris-virginica
    Seq(irisSetosaCount, irisVersicolorCount, irisVirginicaCount)
      // Montrer le nombre d'échantillons pour chaque type d'iris
  )
)

plot(data_iris)
// Afficher le graphique à barres 

val irisSetosa_x = data.filter(_(4) == "Iris-setosa").map(x => x(0).toDouble)
// Extraire les valeurs correspondant à la première caractéristique 
// de l'ensemble des lignes dédiées aux iris setosa
val irisSetosa_y = data.filter(_(4) == "Iris-setosa").map(x => x(1).toDouble)
// Extraire les valeurs correspondant à la seconde caractéristique 
// de l'ensemble des lignes dédiées aux iris setosa

val irisVersicolor_x = data.filter(_(4) == "Iris-versicolor").map(x => x(0).toDouble)
// Extraire les valeurs correspondant à la première caractéristique 
// de l'ensemble des lignes dédiées aux iris versicolor
val irisVersicolor_y = data.filter(_(4) == "Iris-versicolor").map(x => x(1).toDouble)
// Extraire les valeurs correspondant à la seconde caractéristique 
// de l'ensemble des lignes dédiées aux iris versicolor

val irisVirginica_x = data.filter(_(4) == "Iris-virginica").map(x => x(0).toDouble)
// Extraire les valeurs correspondant à la première caractéristique 
// de l'ensemble des lignes dédiées aux iris virginica
val irisVirginica_y = data.filter(_(4) == "Iris-virginica").map(x => x(1).toDouble)
// Extraire les valeurs correspondant à la seconde caractéristique 
// de l'ensemble des lignes dédiées aux iris virginica

val trace1 = Scatter(
    // Créer une instance de la classe Scatter dans le but d'afficher 
    // deux tableaux sous la forme d'un graphique
    irisSetosa_x,
    // Coordonnées x du graphe : un tableau intégrant 
    // la première caractéristique de l'ensemble des iris setosa
    irisSetosa_y,
    // Coordonées y du graphe : un tableau intégrant 
    // la seconde caractéristique de l'ensemble des iris setosa
    mode = ScatterMode(ScatterMode.Markers),
    // Utiliser des marqueurs
    name = "Iris Setosa",
    // Le nom du graphe
    marker = Marker(
        // Couleur et taille des marqueurs et de la ligne 
        // de sélection de chaque marqueur
        color = Color.RGBA(255, 0, 0, 0.8),
        size = 10,
        line = Line(
            color = Color.RGBA(255, 0, 0, 1),
            width = 1
        )
    )
)

val trace2 = Scatter(
    // Créer une instance de la classe Scatter dans le but 
    // d'afficher deux tableaux sous la forme d'un graphique
    irisVersicolor_x,
    // Coordonnées x du graphe : un tableau intégrant 
    // la première caractéristique de l'ensemble des iris versicolor
    irisVersicolor_y,
    // Coordonnées y du graphe : un tableau intégrant 
    // la seconde caractéristique de l'ensemble des iris versicolor
    mode = ScatterMode(ScatterMode.Markers),
    // Utiliser des marqueurs
    name = "Iris Versicolor",
    // Le nom du graphe
    marker = Marker(
        // Couleur et taille des marqueurs et de la ligne 
        // de sélection de chaque marqueur
        color = Color.RGBA(0, 255, 0, 0.8),
        size = 10,
        line = Line(
            color = Color.RGBA(0, 255, 0, 1),
            width = 1
        )
    )
)

val trace3 = Scatter(
    // Créer une instance de la classe Scatter dans le but 
    // d'afficher deux tableaux sous la forme d'un graphique
    irisVirginica_x,
    // Coordonnées x du graphe : un tableau intégrant 
    // la première caractéristique de l'ensemble des iris virginica
    irisVirginica_y,
    // Coordonnées y du graphe : un tableau intégrant 
    // la seconde caractéristique de l'ensemble des iris virginica
    mode = ScatterMode(ScatterMode.Markers),
    // Utiliser des marqueurs
    name = "Iris Virginica",
    // Le nom du graphe
    marker = Marker(
        color = Color.RGBA(0, 0, 255, 0.8),
        // Couleur et taille des marqueurs et de la ligne 
        // de sélection de chaque marqueur
        size = 10,
        line = Line(
            color = Color.RGBA(0, 0, 255, 1),
            width = 1
        )
    )
)

val layout = Layout(
    // Créer une instance Layout dédiée aux propriétés du graphe
    title = "Iris Data",
    // Le titre du graphe
    xaxis = Axis(
        title = "Sepal Length (cm)"
        // Le titre de l'axe des x
    ),
    yaxis = Axis(
        title = "Sepal width (cm)"
        // Le titre de l'axe des y
    )
)

plot(Seq(trace1, trace2, trace3), layout)
// Afficher le graphe et sa mise en page


val irisSetosa_x = data.filter(_(4) == "Iris-setosa").map(x => x(2).toDouble)
// Extraire les valeurs correspondant à la troisième caractéristique 
// de l'ensemble des lignes dédiées aux iris setosa
val irisSetosa_y = data.filter(_(4) == "Iris-setosa").map(x => x(3).toDouble)
// Extraire les valeurs correspondant à la quatrième caractéristique 
// de l'ensemble des lignes dédiées aux iris setosa

val irisVersicolor_x = data.filter(_(4) == "Iris-versicolor").map(x => x(2).toDouble)
// Extraire les valeurs correspondant à la troisième caractéristique 
// de l'ensemble des lignes dédiées aux iris versicolor
val irisVersicolor_y = data.filter(_(4) == "Iris-versicolor").map(x => x(3).toDouble)
// Extraire les valeurs correspondant à la quatrième caractéristique 
// de l'ensemble des lignes dédiées aux iris versicolor

val irisVirginica_x = data.filter(_(4) == "Iris-virginica").map(x => x(2).toDouble)
// Extraire les valeurs correspondant à la troisième caractéristique 
// de l'ensemble des lignes dédiées aux iris virginica
val irisVirginica_y = data.filter(_(4) == "Iris-virginica").map(x => x(3).toDouble)
// Extraire les valeurs correspondant à la quatrième caractéristique 
// de l'ensemble des lignes dédiées aux iris virginica

val trace1 = Scatter(
    // Créer une instance de la classe Scatter dans le but 
    // d'afficher deux tableaux sous la forme d'un graphique
    irisSetosa_x,
    // Coordonnées x du graphe : un tableau intégrant la troisième caractéristique 
    // de l'ensemble des iris virginica
    irisSetosa_y,
    // Coordonnées y du graphe : un tableau intégrant la quatrième caractéristique 
    // de l'ensemble des iris virginica
    mode = ScatterMode(ScatterMode.Markers),
    name = "Iris Setosa",
    // Le nom du graphe
    marker = Marker(
        // Utiliser des marqueurs
        color = Color.RGBA(255, 0, 0, 0.8),
        // Couleur et taille des marqueurs et de la ligne de sélection de chaque marqueur
        size = 10,
        line = Line(
            color = Color.RGBA(255, 0, 0, 1),
            width = 1
        )
    )
)

val trace2 = Scatter(
    // Créer une instance de la classe Scatter dans le but 
    // d'afficher deux tableaux sous la forme d'un graphique
    irisVersicolor_x,
    // Coordonnées x du graphe : un tableau intégrant la troisième caractéristique 
    // de l'ensemble des iris versicolor
    irisVersicolor_y,
    // Coordonnées y du graphe : un tableau intégrant la quatrième caractéristique 
    // de l'ensemble des iris versicolor
    mode = ScatterMode(ScatterMode.Markers),
    name = "Iris Versicolor",
    // Le nom du graphe
    marker = Marker(
        // Utiliser des marqueurs
        color = Color.RGBA(0, 255, 0, 0.8),
        // Couleur et taille des marqueurs et de la ligne de sélection de chaque marqueur
        size = 10,
        line = Line(
            color = Color.RGBA(0, 255, 0, 1),
            width = 1
        )
    )
)

val trace3 = Scatter(
    // Créer une instance de la classe Scatter dans le but d'afficher 
    // deux tableaux sous la forme d'un graphique
    irisVirginica_x,
    // Coordonnées x du graphe : un tableau intégrant la troisième caractéristique 
    // de l'ensemble des iris virginica
    irisVirginica_y,
    // Coordonnées x du graphe : un tableau intégrant la quatrième caractéristique 
    // de l'ensemble des iris virginica
    mode = ScatterMode(ScatterMode.Markers),
    name = "Iris Virginica",
    // Le nom du graphe
    marker = Marker(
        // Utiliser des marqueurs
        color = Color.RGBA(0, 0, 255, 0.8),
        // Couleur et taille des marqueurs et de la ligne de sélection de chaque marqueur
        size = 10,
        line = Line(
            color = Color.RGBA(0, 0, 255, 1),
            width = 1
        )
    )
)

val layout = Layout(
    // Créer une instance Layout dédiée aux propriétés du graphe
    title = "Iris Data",
    // Le titre du graphe
    xaxis = Axis(
        title = "Petal Length (cm)"
        // Le titre l'axe des x
    ),
    yaxis = Axis(
        title = "Petal width (cm)"
        // Le titre l'axe des y
    )
)

plot(Seq(trace1, trace2, trace3), layout)
// Afficher le graphe et sa mise en page

