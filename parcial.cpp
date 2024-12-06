//Precond: "nombre" existe
JBSTNodeStr* find(string nombre, JBSTNodeStr* t){
	JBSTNodeStr* actual = t;
	while(actual->name!=nombre) {
		if(actual->name<nombre) {
			actual = actual->right;
		} else {actual = actual->left;}
	}
	return actual;
} 

//Precond: el arbol dado ya tiene "jefe" (nodo raíz)
void insertar(string nuevo, string superior, JerarquiaBST t) {
	JBSTNodeStr* nodoN = new JSBTNodeStr;
	nodoN->left = nullptr;
	nodoN->right = nullptr;
	nodoN->name = nuevo;
	nodoN->parent = find(superior,t->root);
	JBSTNodeStr* actual = t->root;
	JBSTNodeStr* siguiente;
	if (nuevo > actual->name) {siguiente = actual->right;}
	else {siguiente = actual->left} 
	while(siguiente != nullptr) {
		actual = siguiente;
		if (nuevo > actual->name) {
			siguiente = actual->right;
		}
		else {siguiente = actual->left}	
	}
	if (nuevo > actual->name) {
		actual->right = nodoN;
	}
	else {actual->left = nodoN;}
}


bool esSubordinadoDe(string empleado, string superior, JerarquiaBST t) {
	JBSTNodeStr* actual =find(emplead,t->root);
	while (actual->parent!=nullptr) {
		if(actual->parent==superior) {return true;}
		actual = actual->parent;
	}
	return false;
}
